Small program to sort files in a directory

## Configuration

Configuration is done through a toml file at `~/.config/cleandir/config.toml`. A
default configuration can be created by running `clean-dloads-dir -s`.

```toml
# Top Level tables define directory rules.
[Downloads]

# The direcotry to sort through.
directory = "~/Downloads"

# Relative path to which all files in `directory` will be
# moved. It can include escape sequences that will be expanded
# following the rules of the C library procedure `strftime`.
target-prefix = "%Y-%m"

# Any sub tables define rules for groups of files identified by 
# their extension.
[Downloads.pdf]
enabled = true       # optional key to disable a rule temporarily
extension = "pdf"    # extension of files the rule applies to
target = "PDF"       # subdir of `target-prefix` to use as target

[Downloads.images]
enabled = true

# instead of a single `extension` an array of `extensions` can be 
# given
extensions = ["jpeg", "jpg", "png"]
target = "images"
```

## Building and Installing

To build the program you'll need a working instalation of [chicken
scheme](https://call-cc.org/). You can build and install simply by running

```bash
make && make install
```

By default this will install the program in `~/.local/`, to install somewhere
else e.g. `/usr/` set `PREFIX` accordingly.

This repository comes with a systemd unit and timer, which can be used to run
the programm periodically. The default period is weekly. To activate run

```bash
systemctl --user enable cleandloads.timer 
```

## Contributing

While I do not guarantee any form of support for this program, you can reach me
with patches, suggestions and questions via [mail](mailto:lou+git@repetitions.de).

## TODOs

- more flexible group definitions
- automatic renaming of files
- Date expansion in group rules
- Date expansion based on date-modified instead of local-time


