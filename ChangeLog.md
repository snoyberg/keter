## 1.8.2 

+ Fix XSS issue in the default response.
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
