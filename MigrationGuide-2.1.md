# Migration Guide to `keter` 2.1

## What breaking changes can I expect from this version upgrade?

If your library/application used/referenced/relied on any of the following from `keter` <2.1, _it is now potentially broken with 2.1_:

1. Directories where `keter` and individual application (`app-*`) logs are located.
2. Log file name/format for both `keter` and `app-*` logs.
3. Log file message format for `keter` (NOT including individual `app-*` logs).

## How exactly did things change versus how they were originally?

In the same order:

1. **NEW**: `keter` and individual `app-*` logs are now BOTH located in `log/`  
  
   **OLD**: `keter` logs were located in `log/keter` and individual application logs were located in `log/app-*`.

2. **NEW**: `keter` logs are now named `keter.log` and rotated to `keter.log.1`, `keter.log.2`, ...and so forth
            `app-*` logs are now named `app-*.log` and rotated to `app-*.log.1`, `app-*.log.2`, ...and so forth  
  
   **OLD**: Both `keter` and individual `app-*` logs were named `current.log` within their respective directories and were rotated to 
            `%Y%m%d_%H%M%S.log`. (Following time formatting conventions [as defined by the `time` package](https://hackage.haskell.org/package/time-1.12.2/docs/Data-Time-Format.html#v:formatTime). This was the timestamp when the log was ***rotated out*** of use, not into use.)
3. **NEW**: Log file message format for `keter` logs is now  
   `${time}|${module}:${line_num}|${log_level}> ${msg}`  
  
   **OLD**: Log file message format for `keter` logs used to be  
   `%Y-%m-%d %H:%M:%S.${%q_9} UTC: ${msg}`

Where:
- `${time}` is time formatted as `%Y%m%d_%H%M%S.${%q_2}`.
- `${module}` is the `keter` source module the message was logged from.
- `${line_num}` is the source line number within `${module}` the message was logged from.
- `${log_level}` is the log level (`Info` | `Warn` | `Error` | `Other`).
- `${msg}` is the log message itself.
- `${%q_N}` (where N is 9 or 2 above) is the same as `%q` (picosecond formatting) from the `time` package, but truncated to the first N digits.

## What are some examples of the aforementioned changes?

In the same order:

1. (See below)
2. **NEW**:  
    * `log/keter.log`
    * `log/keter.log.1`
    * `log/keter.log.2`
    * `log/app-blah.log`
    * `log/app-blah.log.1`
    * `log/app-blah.log.2`
 
   **OLD**:  
    * `log/keter/current.log`
    * `log/keter/2023-12-31_245603.log`
    * `log/keter/2023-01-01_010203.log`
    * `log/app-blah/current.log`
    * `log/app-blah/2023-12-31_245603.log`
    * `log/app-blah/2023-01-01_010203.log`
3. **NEW**:
    * `2023-03-11 14:37:17.06|Keter.Main:84|Info> Launching initial`
    * `2023-04-12 09:02:07.92|Keter.Whatever:279|Other> Something something`
   
   **OLD**:
    * `2023-03-11 14:37:17.069101123 UTC: Launching initial`
    * `2023-04-12 09:02:07.921023056 UTC: Something something`

## What suggestions do you have for addressing these breaking changes?

In the same order:

1. Update the directories you are reading the log files from appropriately.
2. Update how you are referencing the actual log file name(s) apppropriately.
     * If you had previously depended on the timestamp in the name of rotated log files, consider parsing the log message timestamps _within_ the individual log files instead. The timestamp of the first log message in any file should accurately tell you at what time logging was ***rotated to*** that file. You can then subtract the number suffixed to its file name by 1 to then find the name of the log file that was ***rotated out*** at that time, if maintaing old semantics is important to you.
3. Update parsing of log messages appropriately.

## Other features you may want to make use of

Stderr logging has now been added! Please consider using this option instead if you are, say, integrating `keter` with `systemd`, which captures stderr output for you. By default, without updating your `keter` config, `keter` will still log to rotating files as usual. As such, this is not a breaking change.

You can enable stderr logging by setting `rotate-logs: false` in your `keter` config.

**NOTE:** When logging to stderr ( _not_ to rotating files), log messages are tagged with the prefixes `keter|` and `app-*>` to distinguish `keter` and individual `app-*` logs, respectively. This is necessary as both types of logs are emitted to the same destination (stderr) unlike when logging to files.

Explicitly stated, stderr log message formats will look like the following:
    
* `keter|${time}|${module}:${line_num}|${log_level}> ${msg}`
* `app-*> ${msg}`

Ex:

* `keter|2023-03-11 14:37:17.06|Keter.Main:84|Info> Launching initial`
* `app-blah> Something something`

Please remember to account for this and not, say, blindly reuse the exact same parsers for both log file messages and stderr log messages. Generally it should suffice to compose a prefix parser in front of the same parser used for the equivalent log file message parser, however.
