---
date: 2022-10-26

title: homophily
subtitle: Is everywhere!
author: Roberto Cantillan

show_post_date: true
show_author_byline: true

draft: false

summary: |
    Follow me on a journey to build this website. The idea is to have a 
    system that has the fewest steps as  possible to go from a blog post on 
    my computer to a website living online. Among other, I discuss about 
    Quarto, Hugo, Github, and Netlify.

format: hugo

freeze: auto
---

I recently noticed that I probably don't spend as much time writing as I
should. *This is the kind of thoughts you can get by doing a
PhD*. This post is me trying to address this problem. Let's write more, let's
blog.

As a first post, I decided to offer a small tour of this website. Or rather,
of how it works. Let's go on an adventure!

# A Fresh New Start

{{< figure src="img/Fig14_A_600.gif" caption="Homophily" >}}

So, to start blogging. The first thing one needs to do so is a website. I had
one, but I wasn't really enthusiastic about it anymore. So all I had to do was
to build a new system that would work for me. Ideally, this system should
be minimal--meaning that it should have as few steps as possible between the
writing of a blog post and its posting, and it should be super easy to show
code running (because that's what I'd like to put here).

In the end, I decided to use some pieces of software that I had used
before and adopt some other. I would use [Quarto](https://quarto.org/)
to write the blog post and the code it contains, [Hugo](https://gohugo.io/) to
generate a static website, and to ditch my old-fashioned Academic theme for
the refreshing [Apéro](https://github.com/hugo-apero/). Here how it works.

## Quarto to Write and Run the Code

You probably won't be surprised to learn that the first step of blogging is
writing. And **I write in what is called Quarto documents**.

[Quarto](https://quarto.org/) is a relatively new piece of tech developed by
the amazing software engineers at [Posit](https://www.rstudio.com/)
(ex-RStudio). Basically, it offers a way to **combine text and code into a
single document**. You can run R, Python, or Julia from one place, and then
decide to turn the annotated results as a PDF file, a markdown document or even a
PowerPoint presentation.

The main reasons I picked Quarto are twofold. It is **familiar** and
**powerful**. First, familiar. Quarto looks a lot like the old
[Rmarkdown](https://rmarkdown.rstudio.com/) format I was used to[^1]. This
reduces by a fair amount what I would have to learn to
build the website. Second, it supports a lot of languages, and even though R
is--and will always remain--my first love, I have to say that I am looking
forward to playing with Python (now that I have figured out how to configure my
environment). Quarto appears to be an amazing tool for people with this
polyglot mindset.

In terms of organization, each of my blog post has its own folder which
contains a Quarto document where I write the post (an `"index.qmd"`). As I said
earlier, this document mixes some writing and some code and is used to
produce an output. When I am done with a post, a `quarto render` command
in my terminal will **produce a document with the code evaluated within**. In
other words, a Quarto containing a code chunk with a `read_csv(...)` call in
it, it will render a document that will show the actual content of the csv I want
to open.

## Hugo to Write the Website

A website, most of the time, is nothing more than some files in [HTML
format](https://en.wikipedia.org/wiki/HTML) put together and served on the
internet. And, **to turn my markdown files into a bunch of files that can be
served**, I decided to use is a
[static website generator](https://en.wikipedia.org/wiki/Static_site_generator)
called [Hugo](https://gohugo.io/).

The job of a static website generator, as the name suggests, is to build a
website whose content won't change very often. This is not the case of your
regular social network that has a feed changing any time you go there. Static
websites are mostly used for product pages, documentation, or blogs. A feature
that is especially interesting with static websites is that they do not require
a [backend](https://en.wikipedia.org/wiki/Frontend_and_backend) to run.
**Once the website built, it can live its life by itself forever**. This
should hopefully reduce the need for me to learn new things to
put the blog online.

So, back to [Hugo](https://gohugo.io/). The basic idea here is that it will
**take my collection of markdown files and turns them into a website**. For
Hugo to do its job, the only thing I had to do is to organize my files onto
my computer in a way that is understable. Basically, my blog posts needed
to be on a `content/` folder, and I had to put a config file in the root
directory.

In a nutshell, all I have to do now is run `hugo server` in a terminal for
**Hugo to build my website** and serve it onto a local server. How convenient?

[^1]: Actually, Quarto and Rmarkdown play well together, and it was a 5 minutes
    job to convert an old Rmarkdown blog post into a new Quarto one.