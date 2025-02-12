# Flexible Timeout Example

Shows how to configure a flexible timeout in keter.

If the flexible timeout is below 10 seconds in this example, keter will
continously try rebooting it because the sleep takes 10 seconds inside `nc.sh`.
