---
layout: post
title: 使用jekyll + github page 搭建博客手记
---


# Table of Contents

1.  [为什么选择 Jekyll + Github Pages](#org1ea5e1c)
2.  [Jekyll 部署](#orgfac91e7)
    1.  [Jekyll依赖工具安装](#orgd4ba59d)
    2.  [jekyll安装](#org39cce32)
    3.  [生成本地博客网站](#orgfc84f4a)
    4.  [设定ruby国内镜像（可选）](#org75c73fd)
    5.  [生成本地博客](#orgba6b521)
    6.  [部署到Github](#orge4ca8bd)
3.  [使用org-mode 写Jekyll博客](#orgcb387c9)


<a id="org1ea5e1c"></a>

# 为什么选择 Jekyll + Github Pages

许多年前，自己也曾经经常写博客。近些年来，由于工作比较繁忙，就一直没有坚持这样好习惯。这次终于下定决心重归博客的怀抱。
网上查阅了许多材料。总体来说现在流行的静态博客生成器有：jekyll,hexo,hugo。

首先，使用这三种生成器都需要一定的git使用知识，而我对git的使用还算比较熟悉。如果使用Jekyll,还需要一点而外的ruby知识，
例如gems, bundle的使用，这也问题不大。Hexo需要node.js的支持，查阅了下别人的资料，大体的安装流程也不算复杂，不过我作为非
专业的程序员，对node.js这个庞然大物有种抗拒感，而hexo依赖node.js，所以可能博客工程会需要一大堆node模块，这样不太符合我品味啊。
hugo是go编写的，其运行速度快应该不容置疑的。hugo的安装也相对比较容易一些，前些天，就是用hugo搭建了一个博客Demo。不过也不太
符合我的品味。

其次，爱折腾的我，当然希望自己能够控制生成的博客的各个角落。hexo和hugo都提供了很成熟的搭建博客的教程，还有一些漂亮的主题。只不过
我喜欢在一个基础配置上改来改去，直到满意为止。hexo和hugo在搭建好，设置好主题之后，基本就没有更改的必要了，这对初学者是极好的。
而使用jekyll，官方文档里的教程也比较丰富，基本跟着文档走，就可以成功地搭建出博客。只不过，个人认为jekyll在github官方加持之下，可以
个人定制的方面要直白的多，直接定制自己需要的css文件和需要需要使用到的gems插件就可以了。这一点上，就可以有更大的操作空间。再一点，
在发布博客方面，需要把markdown文件直接git push到git仓库，github会自动生成网页文件，然后到博客浏览就好了。

最后，作为重度Emacs用户,当然希望通过定制下Emacs插件，通过org-mode写博客咯。


<a id="orgfac91e7"></a>

# Jekyll 部署

Jekyll需要以下环境，这些需要提前安装好，当然在大多数MacOS和Linux系统这些已经安装好了，在Windows需要手动安装。在各个平台安装的详细
步骤可以参考:[Jekyll Installation](https://jekyllrb.com/docs/installation/)

> -   Ruby version 2.4.0 or above, including all development headers (ruby version can be
>
> checked by running ruby -v)
>
> -   RubyGems (which you can check by running gem -v)
> -   GCC and Make (in case your system doesn’t have them installed, which you can check by
>     running gcc -v,g++ -v and make -v in your system’s command line interface)


<a id="orgd4ba59d"></a>

## Jekyll依赖工具安装

首先安装命令行工具。

    xcode-select --install

如果使用的是Mac OS 或Linux，请确定系统安装的ruby版本，因为Jekyll 需要ruby的版本2.4.0以上，以Mac OS为例，Mac OS Mojave 10.14上只安装
了ruby 2.3.x版本，所以还是需要手动安装以下。
如果你没有安装Homebrew,还需要安装以下Homebrew。在Mac OS使用Homebrew也极大的简化安装和管理软件包。安装好Homebrew然后安装ruby的最新版本。

    #查看已经安装的ruby版本，如果版本大于2.4.0，则不需要安装了
    ruby -v

    # 安装homebrew,如果之前没有安装过，执行下面命令
    /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

    #安装最新版本的ruby
    brew install ruby

安装好最新版本ruby后，我们还需要对环境变量进行一些设置，根据homebrew给我们的提示，编辑HOME目录下的.bash<sub>profile,如果使用zsh的话</sub>，就编辑.zshrc.
在文件的末尾增加如下设置：

    export PATH="/usr/local/opt/ruby/bin:$PATH"

    export LDFLAGS="-L/usr/local/opt/ruby/lib"
    export CPPFLAGS="-I/usr/local/opt/ruby/include"

    export PKG_CONFIG_PATH="/usr/local/opt/ruby/lib/pkgconfig"

最后使之生效

    #如果是使用bash
    source .bash_profile

    #若使用zsh
    source .zshrc

检查以下现在使用的ruby版本

    ruby -v
    which ruby
    gem -v
    which gem

如果指向的是homebrew安装软件的目录，（如："/usr/local/opt/ruby/bin/ruby"）。则进行我们下面的步骤。


<a id="org39cce32"></a>

## jekyll安装

现在我们就可以安装Jekyll和Bundler了

    # 下面两条命令选一条
    #安装给本地用户使用
    gem install --user-install bundler jekyll

    #安装全局用户使用
    sudo gem install bundler jekyll

添加gems 环境变量，查看以下ruby的版本号，替换 X.X 版本

    ruby -v
    export PATH=$HOME/.gem/ruby/X.X.0/bin:$PATH


<a id="orgfc84f4a"></a>

## 生成本地博客网站

好了，如果以上安装Jekyll的步骤都一步步完成，下面我们就生成一个博客试一试了。
有两种方式来做：
1.到github找已经做好的jekyll静态博客样式（当然假定已经注册了github账号了），下载来下，放到自己的博客文件夹（如，～/blog）。
Jekyll主题可以在这个网站选择，[Jekyll Themes](http://jekyllthemes.org/),或直接到[Jekyll Wiki](https://github.com/jekyll/jekyll/wiki/sites)挑选别人的配置下载下来，自行更改。当然到Jekyll Themes下载主题来配置相对
简单一些。如果想自行配置，可以建立空白项目，自行调整。
2.使用bundle 生成空白的博客工程。

Jekyll目录结构参考 [Jekyll Directory Structure](https://jekyllrb.com/docs/structure/)
简短描述如下

    |-- _config.yml
    |-- _includes
    |-- _layouts
    |   |-- default.html
    |    -- post.html
    |-- _posts
    |   |-- 2007-10-29-why-every-programmer-should-play-nethack.textile
    |    -- 2009-04-26-barcamp-boston-4-roundup.textile
    |-- _site
     -- index.html


<a id="org75c73fd"></a>

## 设定ruby国内镜像（可选）

在初始化博客网站和安装gems插件可能会遇到网速十分慢的情况，这时可以设置成国内镜像，速度提升不少。
使用 gems中国镜像 <https://gems.ruby-china.com>

    $ gem sources --add https://gems.ruby-china.com/ --remove https://rubygems.org/
    $ gem sources -l
    https://gems.ruby-china.com
    # 确保只有 gems.ruby-china.com

如果你使用 Gemfile 和 Bundler (例如：Rails 项目)

    $ bundle config mirror.https://rubygems.org https://gems.ruby-china.com

这样你不用改你的 Gemfile 的 source。


<a id="orgba6b521"></a>

## 生成本地博客

本地建立博客站点文件夹

    cd
    mkdir blog #博客目录

上文所提到的主题文件等可直接拷贝到这个目录，注意可能需要删除一些文件，例如<sub>post文件夹下的文件</sub>，Gemfile.lock文件（稍后我们自动
生成所需的Gemfile.lock文件），以及调整index.html文件等

    #生成'Gemfile'文件，如果存在就不需要运行下面命令了
    bundle init

    #生成'Gemfile.lock'文件，本地server需要使用
    bundle install

开启Jekyll环境

    bundle exec jekyll serve

Jekyll生成本地站点默认是 'http://127.0.0.1:4000/'。在浏览器打开，就可以预览网站了。

如果直接使用 'jekyll server' 命令运行本地网站，可能会到如下问题
"You must use Bundler 2 or greater with this lockfile. (Bundler::LockfileError)"
这并不是Jekyll有bug，Jekyll对Bundller版本没有限制。如果有任何问题可能是Bundler本身的问题。


<a id="orge4ca8bd"></a>

## 部署到Github

在github新建一个仓库，仓库名称为 username.github.io 。其中'username'就是github的账号名。默认不初始化readme文件。
根据提示，git push到这个仓库就可以了。
不过在push之前，可能需要更改本地的<sub>config.yml</sub>、index.html等文件，将url相关字段修改成 'username.github.io'。
编辑 .gitignore 文件，将一些文件加入里面去
例如：

    .DS_Store
    .jekyll-metadata
    Gemfile.lock
    *.bak
    *.diff
    _site/
    _drafts/
    *.swp
    _org/

使用git关联仓库，并上传到github仓库。

    cd blog
    git init
    git remote origin master git@github.com:username.github.io.git
    git push -u origin master

好了，大功告成。打开浏览器，输入 'https://username.github.io' 查看以下成果吧


<a id="orgcb387c9"></a>

# 使用org-mode 写Jekyll博客

Jekyll支持markdown,textfile等来编写，不过我更习惯使用org-mode。当然是可以使用任何文本编辑器来写博客，如果可以导出到markdown格式，则
直接使用git push到github上也是挺方便的。
使用org-mode写Jekyll博客可以参考官方文档 [Using org to Blog with Jekyll](https://orgmode.org/worg/org-tutorials/org-jekyll.html)

