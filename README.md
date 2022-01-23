At the time of writing, there is no commitment for this project to maintain binary compatibility, or maintenance in general.

## fs2

If you are using fs2, a much cleaner implementation is available to you:
```
  def parMeet[F[_]: Concurrent, G[_]: Foldable, A: Bool: Eq](
    fas: G[F[A]]
  ): F[A] = {
    fs2.Stream
      .foldable(fas)
      .covary[F]
      .mapAsyncUnordered(Int.MaxValue)(x => x)
      .scan(Bool[A].one)(Bool[A].and)
      .takeThrough(x => !Bool[A].isZero(x))
      .compile
      .lastOrError
  }
```

Thanks to SystemFw for this [recommendation](https://discord.com/channels/632277896739946517/843528196876009512/934298321370103909)!
