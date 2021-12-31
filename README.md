# `b2c`

Creates an `iCalendar` file with birthday events based on a vCard 3.0 `vcf` file with contact information.

## What problem does it solve?

Birthday reminders is a good thing to have in a calendar. Unfortunately, [fastmail](https://www.fastmail.com/), which I use, doesn't have a good support for them; it does have [the birthdays calendar](https://www.fastmail.help/hc/en-us/articles/1500000279801-The-birthdays-calendar) that's automatically generated from contacts data, but without any reminder options.

`b2c` is a tiny utility that generates a calendar file with the birthday events from an exported vCard 3.0 file. That calendar can then be imported to fastmail. Thus, it's a more customizable version of the automatic birthdays calendar, but involving more manual steps.

The calendar will have one event for each contact with birthday information; it will start in the year when the program is run and repeat yearly. The event details are currently hardcoded this way: the event title is "&lt;full name&gt;'s Birthday" and an event alert is at 07:00 that day (local time).

## Building

* Install Haskell [`stack`](https://docs.haskellstack.org/en/stable/GUIDE/) using your package manager or the installer script.
* `git clone --recurse-submodules https://github.com/eunikolsky/b2c && cd b2c && stack build`.

## Running

Running `stack install b2c` will install the program to your local `bin` directory (`$HOME/.local/bin` by default, check with [`stack path --local-bin`](https://docs.haskellstack.org/en/stable/GUIDE/#install-and-copy-bins)).

Now it's easy to feed your vCards `vcf` file into the program and save the output to a file:

```bash
$ b2c < ~/contacts.vcf > ~/birthdays.ics
```

That's it, no program options at the moment!

If there is an error parsing the input, the errors will be printed to `stderr` and the exit code will be `1`:

```bash
$ echo -e "BEGIN:VCARD\nVERSION:3.0\nFN:Adam Ford\nBDAY;X-APPLE-OMIT-YEAR=1604:1604-01-01\nEND:VCARD\n" | b2c
5:10:
  |
5 | END:VCARD
  |          ^
vCard must have the N type
$ echo $?
1
```

The program has been manually tested on OS X, and should work on Linux too.

### The Fastmail flow

* [Export the vCard 3.0 file](https://www.fastmail.help/hc/en-us/articles/1500000280061-Import-or-export-contacts#exportcontacts) with all your contacts.
* Run the `b2c` command as described above.
* [Import the generated calendar file](https://www.fastmail.help/hc/en-us/articles/360060590773-Import-export-your-calendars#importcalendars) to a new calendar.

### Known issues

This initial version is a proof-of-concept, so there are known issues:

* The vCard parser handles [unfolding](https://datatracker.ietf.org/doc/html/rfc2425#section-5.8.1) in a limited way, so not all `vcf` files can be parsed at this time.
* The program doesn't parse any options to override the hardcoded settings (i.e., alert time, event title).
* There are few unit tests.

## Tests

`stack test` runs the unit tests.

Note: you can plug your own `vcf` file into the tests by creating a symlink called `test/contacts.vcf` pointing to your file:

```bash
$ ln -sv ~/Downloads/All\ Contacts.vcf test/contacts.vcf
```

If it's found, a unit test will verify that it is parsed correctly.
