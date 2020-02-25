## 1.5

* Builds with `process` 1.6
* add dependency for `tls-session-manager`, and it's support
* add support for ssl certs for redirects, static-files, reverse proxies
* fix http2 on reverse proxies
* make it compile with ghc-8.6.3

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
