# MoscoviumOrange
![Screenshot](demo.jpg)

A more feature full command line history logger/viewer. It logs the command, path and timestamp.

Some examples:

- To view the history of all commands in the current directory or it's children:
  ```
  moscoviumorange --path-prefix=$PWD
  ```

- To view the history of all commands that had `curl` anywhere.
  ```
  moscoviumorange --command-contains=curl
  ```

There are options to filter the results by:

- path prefix/suffix/anywhere
- command prefix/suffix/anywhere
- before / after (time)

I've tried to make it efficient by only writing to the filesystem if there are new entries, as well as only writing every n seconds.

## Installation

### Nix
nix-env -i -f https://github.com/chrissound/MoscoviumOrange/archive/master.tar.gz

### Others

Probably install cabal / stack and then build it from there.

## Help 
```
moscoviumorange --help  
General program title/description

Usage: moscoviumorange ([--print] [--limit NUMBER] | [--print-filter]
                       [--path-contains STRING] [--path-prefix STRING]
                       [--path-suffix STRING] [--path STRING]
                       [--command-contains STRING] [--command-prefix STRING]
                       [--command-suffix STRING] [--command STRING]
                       [--before STRING] [--after STRING] [--limit NUMBER] |
                       [--daemon])

Available options:
  -h,--help                Show this help text
  --print                  Whether to be quiet
  --limit NUMBER           limit
  --print-filter           Print with filter
  --path-contains STRING   command
  --path-prefix STRING     command
  --path-suffix STRING     command
  --path STRING            path equals
  --command-contains STRING
                           command
  --command-prefix STRING  command
  --command-suffix STRING  command
  --command STRING         command equals
  --before STRING          command
  --after STRING           command
  --limit NUMBER           limit
  --daemon                 Run daemon listener
```

## Daemon

Just run `moscoviumorange --daemon`.

With Nixos:

```
let

  moscoviumorange = pkgs.callPackage ??????????????? {};
  in
...
```

```
  systemd.user.services = {
    moscoviumOrange = {
      enable = true;
      description = "MoscoviumOrange terminal history";
      serviceConfig = {
        Type      = "simple";
        ExecStartPre = "${pkgs.coreutils}/bin/mkdir -p %h/.config/moscoviumOrange/pending/";
        ExecStart = "${moscoviumorange}/bin/moscoviumorange --daemon";
        Restart   = "always";
        RestartSec   = 1;
      };
      wantedBy = [ "default.target" ];
    };
  };
```

# Configure with ZSH

Requires: jq + BSD netcat

```
function printc () {
  # (nohup ~/ScriptsVcs/addEntryToMoscoviumOrange.sh "$1" &) > /dev/null 2>&1 # 
  if [ -S ~/.config/moscoviumOrange/monitor.soc ]; then
      $(jq -n --arg command "$1" --arg path "$PWD" '{"command":$command, "path":$path}' | "$(echo 'readlink -f $(which nc)' | nix run nixpkgs.netcat)" -N -U ~/.config/moscoviumOrange/monitor.soc &)
  else
      echo "Failed to log command to moscoviumorange" > /dev/stderr
  fi
}

add-zsh-hook preexec printc
```
