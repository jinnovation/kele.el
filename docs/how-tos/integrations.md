# Integrations

Kele contains several integrations with select packages.

## awesome-tray

[`awesome-tray`](https://github.com/manateelazycat/awesome-tray) "folds" the
modeline into the minibuffer for a compact UI.

Kele ships with `awesome-tray` integration that will display the current context
and namespace in your "modeline."

![](./img/awesome-tray.png)

To enable, simply add `"kele"` to your desired place in
`awesome-tray-active-modules`, like so:

```emacs-lisp
(add-to-list 'awesome-tray-active-modules "kele" t)
```
