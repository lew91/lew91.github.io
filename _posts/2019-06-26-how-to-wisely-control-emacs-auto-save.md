---
title: How to wisely control Emacs's auto save
layout: post
categories: [Emacs] 
tags: 
---

# Table of Contents

1.  [Why introduce this?](#orgb8c6f77)
2.  [what can I do with this satiation](#orga232a5e)
3.  [How to control the auto-save more wisely?](#org66fbf58)


<a id="orgb8c6f77"></a>

# Why introduce this?

Did you find out that when you use Emacs save some files or even your Emacs had crashed, there have some files like this
"foo~" or "#foo#". Those happend because Emacs's **auto-save** and **backup** features. You maybe annoyed by the production of those
files. 

Emacs has two helpful features, called *auto-backup* and *auto-save*.

Auto-backup is triggered when you save a file - It will keep the old version of the file around, adding
a ~ to its name. So if you saved the file *foo*, you'd get *foo~* as well.

*auto-save-mode* auto-saves a file every few  seconds or every few characters (both settings are configurable

-   *auto-save-interval* is set to 300 characters by default and /auto-save-timeout is set to 30 seconds).

The auto-save files have names like #foo# and are deleted  automatically when you manually save a file.


<a id="orga232a5e"></a>

# what can I do with this satiation

Although the modes are definitely useful, many Emacs users find the extra files they create quite annoying
(especially since they rarely resort to using them) and disable both feature to get rid of the pesky 
unwanted files:

{% highlight emacs-lisp %}
;; disable auto-save and auto-backup
(setq auto-save-default nil) ; stop creating backup~ files
(setq make-backup-files nil) ; stop creating #autosave# files
{% endhighlight %}

Even though I've never actually had any use of those  backups, I still think it's a bad idea to disable them.
I find it much more prudent to simply get them out of sight by storing them in the OS's *tmp* directory instead.

{% highlight emacs-lisp %}
;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
{% endhighlight %}

Now they won’t annoy you constantly, but they will still be around if you need them. Some OSes delete everything 
in their tmp directories on restart, so if this worries you - consider using another directory.

Like in the *user-emacs-directory*, make a new directory named ".backup", all the auto save and backup files will 
contained in this folder.

{% highlight emacs-lisp %}
;; Auto save files and backup files, the backup files sometimes are usefull.
(setq backup-directory-alist `((".*" . ,(locate-user-emacs-file ".backup")))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
{% endhighlight %}


<a id="org66fbf58"></a>

# How to control the auto-save more wisely?

Since we have learn some Elisp programming language. Let's write our own save functions.

First we should write the variable that how often we do auto-save. like this:

{% highlight emacs-lisp %}
(defvar save-idle-timer 5)
{% endhighlight %}

Our auto save main function write like this:
{% highlight emacs-lisp %}
(defun save-command ()
  "Save the current buffer."
  (when (and buffer-file-name
             (buffer-modified-p (current-buffer))
             (file-writable-p buffer-file-name)
    (save-buffer)))
{% endhighlight %}

If we want to save all the files one time that we have opened. The functions like this:

{% highlight emacs-lisp %}
(defun save-all-buffers ()
  (save-excursion
    (dolist (buf (buffer-list))
      (set-buffer buf)
      (when (and buffer-file-name
                 (buffer-modified-p (current-buffer))
                 (file-writable-p buffer-file-name)
        (save-buffer)))))

(advice-add 'save-command :override 'save-all-buffers)
{% endhighlight %}

Then, we come to our finally function. It work that auto-save.

{% highlight emacs-lisp %}
(defun my-auto-save ()
  (run-with-idle-timer save-idle-timer t #'save-command)
  (add-hook 'before-save-hook 'font-lock-flush))
{% endhighlight %}

That all. 
