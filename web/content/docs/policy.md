+++
title = "Public admission and rule order"
description = "A public version needs a decisive allow. Rules run in precedence order, and the first decisive result wins."

weight = 39

[extra]
nav_title = "Public admission and rules"
group = "Detailed behaviour"
kind = "Reference"
reference = "/docs/configuration/#rule-policy"
+++

## What the default policy does {#defaults}

| Condition | Default outcome |
| --- | --- |
| Public version is at least seven days old | The age rule can admit it if no earlier rule decides otherwise. |
| npm version has install-time execution | The npm admission rule denies it. This default does not remove stored copies. |
| No advisory source configured | The default known-malicious and remediation rules stay inactive. |
| Advisory source configured | Known-malicious denial and qualifying remediation admission become active. |
| No rule allows the version | Public admission is denied. |
| Version is a trusted private copy | Public admission rules do not run. Private access and applicable integrity/completion checks still apply. |

## An allow does not mean every check passed {#order}

Higher precedence runs first. A decisive allow can prevent lower rules from running. A check that was not reached is not a successful check.

An identity allow at its default precedence overrides scored CVSS and EPSS denies. It does not override the default known-malicious or npm install-script denies.

Print the resolved order with `check-config`. Use that order when reviewing an exception.

## How age is measured {#age}

npm uses the version’s publication time. PyPI uses the newest file upload in the release, so adding a file can restart its waiting period.

A missing or malformed PyPI file timestamp leaves the release age unknown. The age rule then cannot admit it. An explicit exception still needs to satisfy other applicable checks.

## Policy is not the only check {#integrity}

Public files must satisfy the artifact host and integrity requirements. The default public digest floor is `sha256`.

Files that fail these checks disappear from listings. A release disappears when no eligible file remains. An allow rule does not waive those checks.

PyPI does not support the npm scope, deprecation, or install-execution rule types. An active incompatible rule prevents that mount from starting.

{{ task_link(path="@/docs/exceptions.md", title="Allow a specific version", description="Choose the override deliberately and inspect its effect.") }}

{{ task_link(path="@/docs/revocation.md", title="Admission and revocation", description="See when a rule can remove an existing copy.") }}
