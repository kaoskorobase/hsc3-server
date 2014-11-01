### [v0.9.2](https://github.com/kaoskorobase/hsc3-server/tree/v0.9.2)

* Add changes in CHANGELOG

### [v0.9.1](https://github.com/kaoskorobase/hsc3-server/tree/v0.9.1)

* Update `hsc3` to version 0.15

### [v0.6.0](https://github.com/kaoskorobase/hsc3-server/tree/v0.6.0)

* Add `Control.Failure.Failure` instance to `Sound.SC3.Server.State.Monad.Server`
* Add `hostname` parameter to `Sound.SC3.Server.State.Monad.Process.withTransport`

### [v0.5.0](https://github.com/kaoskorobase/hsc3-server/tree/v0.5.0)

* Use distinct types for audio and control buses and IDs
* Factor monad type classes into `Sound.SC3.Server.Monad.Class`
* Remove `Sound.SC3.Server.Monad.capture` combinator
* Upgrade to `hosc` 0.13 and `hsc3` 0.13
* Move `Sound.SC3.Server.Monad` hierarchy to `Sound.SC3.Server.State.Monad` to avoid a clash with `hsc3`
* Rename `Sound.SC3.Server.Monad.Send` to `Sound.SC3.Server.State.Monad.Request` and refactor interface and semantics
* Export `withTransport` from `Sound.SC3.Server.State.Monad.Process`

### v0.4.0

* Move allocation interface to `Sound.SC3.Server.Monad` and add monad control
  instances: Remove allocation interface from `Sound.SC3.Server.Connection` and
  move it to `Sound.SC3.Server.Monad`.
* Remove `data-accessor` dependency.
* Add instances for `MonadBase`, `MonadBaseControl` and `MonadTransControl`,
  allowing to lift control operations from the base monad into ServerT.
* Implement `Sound.SC3.Server.Monad.fork` in terms of
  `Control.Concurrent.Lifted.fork`. `fork` may be removed from the interface in
  future versions.

### [v0.3.0](https://github.com/kaoskorobase/hsc3-server/tree/v0.3.0)

* Add new modules `Sound.SC3.Server.Monad.Send` and `Sound.SC3.Server.Monad.Command` providing abstractions for synchronous and asynchronous commands and completion messages
