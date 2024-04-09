# A simple HTTP client/server (http/1.1 & h2) with [Miou][miou]

![httpcats](./httpcats.gif)

```ocaml
$ git clone https://github.com/robur-coop/httpcats
$ cd httpcats
$ opam pin add -yn .
$ dune build app/pars.exe
$ cat >to_download.txt <<EOF
https://builds.osau.re/job/bob/build/latest/f/bin/bob.com
EOF
$ dune exec app/pars.exe -- < to_download.txt
```

**NOTE**: it requires the upstream version of `miou` and it pins few packages
like `mirage-crypto` that we fixed but not yet released.

- [ ] Implement some tests
  + [x] tests with `http/1.1` and without TLS (`GET` and `POST` with stream)
  + [ ] tests with TLS
- [ ] Documentation (.ml & .mli)
- [ ] DNS resolution over UDP
- [ ] DNS over TLS

[miou]: https://github.com/robur-coop/miou
