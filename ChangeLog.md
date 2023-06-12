## 2.1.1

+ Bump unix-compat bound to accept 0.7

## 2.1

Please reference `MigrationGuide-2.1.md` for in-depth documentation on breaking changes to be aware of, examples of said changes, and potential solutions/workarounds for them if you plan on upgrading to this version of `keter`.

+ Log naming and directory scheme has changed for both main keter logs and app logs.  
  Old logs were named `dir/current.log` for the current log and `%Y%m%d_%H%M%S.log` 
  (`time` package conventions) for rotated logs.  
  Current logs have been brought up one level and named after their old directory:  
  `logs/keter/current.log` -> `logs/keter.log`  
  Rotated logs will now simply have `.1` `.2` ascending appended to the name of the base logs 
  rather than be named after the date and time they were rotated at:  
  `logs/keter/20230413_231415.log` -> `logs/keter.log.1`  
  `logs/__builtin__/20230413_231415.log` -> `logs/__builtin__.log.1`  
  `logs/app-foo/20230413_231415.log` -> `logs/app-foo.log.1`  
  Please update anything that depended on the old log naming and directory conventions accordingly.
+ Added the `rotate-logs` option (default: true) in the keter config file.  
  When true, the main keter (non-app!) logs will rotate like they have in previous versions.  
  When false, the main keter logs will emit straight to stderr; this is useful e.g. if you're 
  running keter via systemd, which captures stderr output for you.
+ Internal logging implementation has been switched over to `fast-logger` instead of the
  old in-house logging solution.  
  Please be aware that compared to the old logging solution, the usage of `fast-logger` does
  not 100% guarantee consistent time ordering. If anything previously depended on tailing the last
  line of a log and critically assumed that messages will be in order, it should now parse via 
  timestamp instead.
+ The `LogMessage` ADT has been removed.
+ Replaced individual logging calls with TH splice -style calls where sensible to have access to source location info.
+ Updated log message format to make use of the additional info:  
  `"$time|$module$:$line_num|$log_level> $msg"`
+ Added `Keter.Context`, exposing the new `KeterM` monad and related functions.  
  This monad carries a mappable global config and logger around, eliminating the need to pass various configuration data and the logger to everything.
+ Refactored most `Keter.*` module functions to be actions in `KeterM`  
  Please anticipate breaking changes for anything written against the exposed API.

## 2.0.1

+ Force usage of http-reverse-proxy versions above 0.6.0.1.
  This prevents a DoS attack on a head request followed by a post.

## 2.0

+ Improve missing sudo error messages in postgres plugin.
+ Reorganized most Haskell source files into /src.
+ Dropped support for http-client < 0.5.0.
+ Removed 'default' package.
+ All "Data" modules are now "Keter" modules.
+ Testing library switched from "hspec" to "tasty".
* Move Network.Http.ReverseProxy.Rewrite into Keter.Rewrite
* Move Codec.Archive.TempTarball into Keter.TempTarball
* Hide Keter.Aeson.KeyHelper
* Stop re-exporting common and rewrite from types
* Common no longer re-exports half of Haskell
* Rename Types to Config
* Move Common out of Config into root

## 1.9

+ Update status code of missing host responses.
  They now emit a 502 on missing host, and 404 on host not found
+ Always restart keter in the nix config for systemd.
  It turns out that keter may exit with exit code 0 under load testing.
  Changing from on-failure to always in systemd should bring it back up.
+ Squash proxy exceptions if they occur and serve a default or custom error
  response.
  Emits the exception to the log.
+ Add incoming folder to the CI build.

## 1.8.4

+ Get rid of ominious warning at the top.
  Thanks to /u/Opposite-Platypus-99
  for pointing this out.

## 1.8.3

+ HTML escape X-forwarded-host response as well.

## 1.8.2 

+ Fix XSS issue in the default response for host not found.
  (special thanks to Max @ulidtko for spotting and fixing this)

## 1.8.1

+ Fix haddock build

## 1.8

+ Add NixOS support
+ Describe debug port in readme.
+ Improve ensure alive error message due to 
  https://github.com/snoyberg/keter/issues/236
+ Add `missing-host-response-file` and `unknown-host-response-file`
  to the global keter config, which replace the default responses.
+ All missing-host responses will now fill the requested host in the
  `X-Forwarded-Host: HOSTNAME` header, where HOSTNAME is the requested host.
  This is done because the default response fills in the hostname.
  Now javascript could potentially fix that by making another request
  to itself.
+ Document missing configuration options in `etc/keter-config.yaml`

## 1.7

* Add support Aeson 2.*
* Add `Data.Aeson.KeyHelper.hs` in cabal file.
* And use the module where Aeson has changed how to handle Key and KeyMap.

## 1.6
* Make keter more chatty on boot.
  This allows you to figure out in code where things go wrong.
* Add opt-in debug CLI, allowing you to inspect keters' internal state.
  You can activate it by specifying a cli-port.
* Emit which pid is being killed by keter.
  This helps with process leakage issues,
  for example if the user launches from a bash script without using `exec`.

## 1.5

* Builds with `process` 1.6
* add dependency for `tls-session-manager`
* Add show instance for App
* Add ensure alive timeout config
* Add `nc` example in incoming
* Change to github actions because travis ci stopped working.
* Fix hackage issues in cabal file
* Fix breaking changes with warp-tls.

## 1.4.3.1

* Add cabal flag `system-filepath` for compatibility with older versions of fsnotify.

## 1.4.3

* Update fsnotify dependency version and remove system-filepath.

## 1.4.2.1

Bug fix: Change default connection time bound from 5 sec to 5 minutes [#107](https://github.com/snoyberg/keter/pull/107)

## 1.4.1

* Add configurable timeouts [#93](https://github.com/snoyberg/keter/pull/93)

## 1.4.0.1

* Avoid infinite loop traversing incoming directory [#96](https://github.com/snoyberg/keter/issues/96)

## 1.4.0

* Drop system-filepath

## 1.3.10

* Configurable time bound [#92](https://github.com/snoyberg/keter/pull/92)

## 1.3.9.2

* Lower case PostgreSQL names [#88](https://github.com/snoyberg/keter/pull/88)

## 1.3.9.1

* Allow blaze-builder 0.4

## 1.3.9

* Support chain certificates in credentials [#82](https://github.com/snoyberg/keter/pull/82)

## 1.3.7.1

Bug fix: catch exceptions during reload [#64](https://github.com/snoyberg/keter/issues/64)

## 1.3.7

* Add ability to use middleware [#63](https://github.com/snoyberg/keter/pulls/63)

## 1.3.6

Support the `forward-env` setting.

## 1.3.5.3

More correct/complete solution for issue #44. Allows looking up hosts either with or without port numbers.

## 1.3.5.2

Partial workaround for keter.yaml files that give a port with the hostname.

## 1.3.5.1

Fix bug where the cleanup process would remain running.

## 1.3.5

All stanzas may have the `requires-secure` property to force redirect to HTTPS. You can set additional environment variables in your global Keter config file.

## 1.3.4

Support for overriding external ports. Support for keter.yml in addition to keter.yaml. Case insensitive hostname lookups.

## 1.3.3

Set the X-Forwarded-Proto header

## 1.3.2

Enable GZIP middleware

## 1.3.1

Upgrade to WAI 3.0

## 1.3.0

Upgrade to conduit 1.1

## 1.0.1

Permit use of wildcard subdomains and exceptions to wildcards. Convert internal strings to use Data.Text in more places. (Although internationalized domain names are not supported unless entered in punycode in configuration files.)

## 1.0.0

Significant overhaul. We now support monitoring of much more arbitrary jobs (e.g., background tasks), have a proper plugin system (PostgreSQL is no longer a required component), and have a much better system for tracking hostname mapping changes.

## 0.4.0

Switch to fsnotify to get cross-platform support. No longer using raw proxies, but instead WAI proxies.

## 0.3.7

Sending a HUP signal reloads the list of deployed apps. This is useful for circumstances where inotify does not work correctly, such as on file systems which do not support it.

## 0.3.5

You can now create Keter bundles without any applications. These can contain static hosts and redirects.
