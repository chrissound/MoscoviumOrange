# MoscoviumOrange (WORK IN PROGRESS / experimental)

![Screenshot](demo.jpeg)

A better way to log your command line history. It logs the path + time + command.

You can view records by `moscoviumorange --print`. There are options to filter the results by:

- path prefix/suffix/anywhere
- command prefix/suffix/anywhere
- before / after (time)

I've tried to make it efficient by only writing to the filesystem if there are new entries, as well as only writing every n seconds.

# Installation

### Nix
nix-env -i -f https://github.com/chrissound/MoscoviumOrange/archive/master.tar.gz

### Others

Probably install cabal / slack and then build it from there.

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
