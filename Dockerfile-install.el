(progn
  (package-initialize)
  (package-refresh-contents)
  (package-install 'elnode)
  (customize-set-variable 'elnode-init-host "0.0.0.0")
  (customize-set-variable 'elnode-do-init t)
  (customize-save-customized))
