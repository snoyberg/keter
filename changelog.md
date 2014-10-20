__1.3.5__ All stanzas may have the `requires-secure` property to force redirect to HTTPS. You can set additional environment variables in your global Keter config file.

__1.3.4__ Support for overriding external ports. Support for keter.yml in addition to keter.yaml. Case insensitive hostname lookups.

__1.3.3__ Set the X-Forwarded-Proto header

__1.3.2__ Enable GZIP middleware

__1.3.1__ Upgrade to WAI 3.0

__1.3.0__ Upgrade to conduit 1.1

__1.0.1__ Permit use of wildcard subdomains and exceptions to wildcards. Convert internal strings to use Data.Text in more places. (Although internationalized domain names are not supported unless entered in punycode in configuration files.)

__1.0.0__ Significant overhaul. We now support monitoring of much more arbitrary jobs (e.g., background tasks), have a proper plugin system (PostgreSQL is no longer a required component), and have a much better system for tracking hostname mapping changes.

__0.4.0__ Switch to fsnotify to get cross-platform support. No longer using raw proxies, but instead WAI proxies.

__0.3.7__ Sending a HUP signal reloads the list of deployed apps. This is useful for circumstances where inotify does not work correctly, such as on file systems which do not support it.

__0.3.5__ You can now create Keter bundles without any applications. These can contain static hosts and redirects.
