---
layout: post
title: How to play Emacs happily
categories: [Emacs,Blog]
---
{{ page.title }}
==================
<p class="meta">16 Jun 2019</p>



I have been using Emacs for a long time,since from my college. although I can't be very proficient in Emacs,
but some basic configurations are still able to deal with. Maybe I should starting write some Emacs extensions
for fun.
From 2009 to 2014, I did not use Emacs at all.During that time, I worked hard on e-commerce. Those years, my use
of Emacs has been repeated, given up for a while, and then picked up. Maybe it’s such an experience，let me better
 understand the difficulty of using Emacs. So it's time to write an post about Emacs，it's can an be regarded
as a summary of myself.

**Why give up and then pick it up?**

As well known, use Emacs can speed lots of time to config it. Learning Emacs costs so high, even in china there has
such a saying "Vim is the  god of editor and Emacs is a god's editor". Even if you hear such a statement, you will
 smile. But this reflects the fact that learning both editors is not easy.
Here is a most common editor, and it's learning curves.
![img](http://ergoemacs.org/emacs/i/emacs_learning_curves.png)
Second, it took too much time on how to configure it, unlike some out-of-the-box editors that have been configured.
Finally it's seems worked  and may be crashed by a signal plugin. That is simply unacceptable.

But meanwhile, Emacs has some advantages that other editors can't match. You can almost customize any of Emacs' features,
such as shortcut key bindings, listening to music, browsing the web, and even using Emacs to brew coffee. As refer to
the official documentation, Emacs is an operation system that can do whatever you want. It just need to have enough
imagination.The Emacs community is where hackers gather to learn a variety of things. Emacs is a model for the open
source community and the brainchild of the world's top hackers. So learning Emacs is a step closer to the top.

**How learning Emacs smoothly**

Firstly，we should understand the order of Emacs loading files. When Emacs is started, it normally tries to load a
lisp program from an initialization file, or init file for short.This file, if it exists, specifies how to initialize
Emacs for you. Emacs looks for your init file using the filenames '~/.emacs', '~/.emacs.el', or '~/.emacs.d/init.el'.
Then, It's find the load-path, load other config files.
How to use the feature, I recommends use the follow code to control Emacs load actions. Make Emacs load the designation
directory, Once a file or plugin goes wrong, we can temporarily remove the folder of this plugin in the configuration file.

    (defun add-subdirs-to-load-path (dir)
      "Recursive add directories to `load-path'."
      (let ((default-directory (file-name-as-directory dir)))
        (add-to-list 'load-path dir)
        (normal-top-level-add-subdirs-to-load-path)))

    ;; add a plugin to the load-path
    (add-subdirs-to-load-path "~/.emacs./XXXX")

**How install packages**

There has some ways to install packages, use 'package.el','use-packages' macro package, or even install package manually.
Here is my example to install some packages. I use this, because I found it's easily to control which packages install and
uninstall without hide much more details.

    (require 'package)
      (add-to-list 'package-archives
                   '("melpa" . "https://melpa.org/melpa/"))
      (package-initialize)
      (when (not package-archive-contents)
        (package-refresh-contents))

      (defvar mypakcages
        '(
          elpy
          better-defaults
          ))

      (mapc #'(lambda(package)
                (unless (package-install-p package)
                  (package-install package)))
            mypackages)

**How bindding keys in different platform**

In Mac OS i want change the default Meta and Option key, and In Windows i want use the Win key as super key. here is the
code.

    (cond ((featurep 'cocoa)
           ;; switch option and command key
           (setq mac-option-modifier 'super)
           (setq mac-command-modifier 'meta)
           (setq mac-right-command-modifier 'ctrl)
           )

          ((eq system-type 'windows-nt)
           ;; Make PC keyboards Win key or other to type Super or Hyper
           (setq w32-pass-lwindow-to-system nil)
           (setq w32-lwindow-modifire 'super)
           (setq w32-apps-modifier 'hyper)

           (w32-register-hot-key [s-])
           (w32-register-hot-key [s-t])
           )
          )

Normally, set keys for a special mode and unbind a set (for any keymap) like this.

    (define-key KEYMAP KEY)

    ;; unbind key
    (define-key KEYMAP KEY nil)

    ;; global set key for some function or mode
    (global-set-key (kbd "KEY") 'XXX-XXX-XXX)

For converniecnce, there are also two standard functions for undbinding from the glbobal keymap and from the local keymap
(which is uausally the major mode's kemap).

-   (global-unset-key KEY)
-   (local-unset-key KEY)

Those ones are interactive commands, as per their respective complemets *global-set-key* and *local-set-key*

**How to find BUG or config wrong setting in configuration files**

Sometimes, there  appear some errors in our Emacs configuration files.We can start Emac with the following shell command，to find
error message.

    Emacs --debug-init

After we get the error message, we need to copy the error message on the top line. First search for EmacsWiki and Google
to see if there are other problems and the same problem. If you have the same problem, you usually have a solution. If
you can search for, understand and solve the problem yourself, don't be a party. This is a necessary process for learning
to improve, and it is also one of the fun of tossing.

If you can't find a solution, just copy the top line of the error message and copy the context of about 20 lines, then
drag the plug-in process yourself (from where to download the plug-in, how to load the configuration file, and the error
encountered) Information, ideas for trying to solve the solution yourself, such as those already searched on EmacsWiki
and Google), are pasted into the Emacs mailing list and Emacs IRC channel (#emacs 6667 irc.freenode.net) (IRC channel
has always been the rule to avoid pasting large sections of text into In the channel, the IRC channel is recommended
to paste to paste2.org, paste the sharing site, paste the shared address to the IRC channel to respect other people
on the IRC channel, and then wait for the help of the community experts, as long as you are brainstorming, even Your
ability is a little bit more enthusiastic masters are willing to help, do not be a grandfather's reach, especially you
are facing the world's top players.

**What's the Next**

After config Emacs basically. It is time to start learning Emacs-lisp and some complex plugins.First familiar with the
shortcuts for Emacs operations. Read Emacs-lisp reference manual to familiar with the Emacs-lisp programming language.
Install some complex plugins, try to config it and modify it, fix bug in your config files. Make some interesting wheels.
If you write the Elisp plugin yourself, you will find that Emacs is actually easier to use. Because it includes regular
expressions, syntax highlighting, patterns, asynchronous child processes, hooks, overlays, advice. After you write
familiar, you will find that the only difference between all plugins is the difference between complexity and imagination.
Something that can't be written. At this stage, the only thing that allows you to learn more is to go to IRC #emacs and talk
to some old hackers, or go to GitHub Emacser. Learn C language, and use C or Elisp to contribute to the underlying Emacs
directly in your spare time.
