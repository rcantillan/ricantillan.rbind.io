---
title: A Blog That Works
description: |
  This is a fully featured blog that supports categories, 
  tags, series, and pagination.
author: "The R Markdown Team @RStudio"
show_post_thumbnail: true
thumbnail_left: true # for list-sidebar only
show_author_byline: true
show_post_date: true
show_button_links: false
# for listing page layout
layout: list-sidebar # list, list-sidebar, list-grid

# for list-sidebar layout
sidebar: 
  title: Notes Junction
  description: |
    "Step into a realm where the quantitative social network analysis is illuminated through the powerful lens of the R language. Here, programming intertwines with abundant data extracted from egocentric and sociocentric networks. Embark on an enlightening journey that delves into topics such as Social Capital, Social Inequalities, and Social Network Analysis—a perspective tailored to unveil the multifaceted layers of societal dynamics. This landscape provides a gateway to a sophisticated comprehension of societal structures and fervently encourages you to share and enhance your insights openly." 
  author: "Roberto Cantillan"
  text_link_label: Subscribe via RSS
  text_link_url: /index.xml
  categories_link: true
  series_link: true
  tags_link: true
  show_sidebar_adunit: true # show ad container

# set up common front matter for all pages inside blog/
cascade:
  author: "The R Markdown Team @RStudio"
  show_author_byline: true
  show_post_date: true
  show_comments: true # see site config to choose Disqus or Utterances
  # for single-sidebar layout
  sidebar:
    text_link_label: View recent posts
    text_link_url: /blog/
    show_sidebar_adunit: false # show ad container
---

** No content below YAML for the blog _index. This file provides front matter for the listing page layout and sidebar content. It is also a branch bundle, and all settings under `cascade` provide front matter for all pages inside blog/. You may still override any of these by changing them in a page's front matter.**
