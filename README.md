emacs configuration

Modify for findk and grepk and M-x byte-compile-file later on
find:
/usr/share/emacs/24.5/lisp/find-dired.el.gz
1. remove " . " everywhere
2. replace 'lookfor-dired' argument -> dir with -> " " for findk to work propery

grep:
/usr/share/emacs/24.5/lisp/progmodes/grep.el.gz
1. comment/remove setenv for GREP_OPTIONS
2.

    (setenv "PATH" (concat (getenv "PATH") ":~/.local/bin"))
    (setq exec-path (append exec-path '("~/.local/bin")))