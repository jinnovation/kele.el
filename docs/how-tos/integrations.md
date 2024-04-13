# Integrations

Kele contains several integrations with select packages.

## doom-modeline

Kele is integrated with
[`doom-modeline`](https://seagle0128.github.io/doom-modeline/). You can add a
`k8s` segment to your modeline that displays the currently active context and,
optionally, its default namespace. You can also access the menu bar from it.

![](./img/doom-modeline.png)
![](./img/doom-modeline-menubar.png)

To get started, simply define a custom modeline with the `k8s` segment in it and
set it as the default. For example:

```emacs-lisp
(doom-modeline-def-modeline 'kele-modeline
  '(bar matches buffer-info remote-host buffer-position parrot selection-info)
  '(misc-info k8s minor-modes input-method buffer-encoding major-mode process vcs checker))

(add-hook 'doom-modeline-mode-hook
          (lambda ()
            (doom-modeline-set-modeline 'kele-modeline 'default)))
```

See [`doom-modeline` documentation](https://seagle0128.github.io/doom-modeline/)
for more nuanced use cases.

