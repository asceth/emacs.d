(setq-default grep-highlight-matches t
              grep-scroll-output t)

(when *is-a-mac*
  (setq-default locate-command "mdfind"))

(provide 'init-grep)
