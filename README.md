# daniel-finnan.github.io

# To Do

- Styling
    - add customisation

Renamed `assets` and `_sass` folder to `..._OLD` temporarily to store old styling.

Trying to drop in new CSS from Minima 2.5.2 theme.
Changed link to css file in `head.html`
Altered `header` to apply correct styling, notably using `nav-items` and `nav-item` from 2.5.2 version, pulled out from `nav-items`.
Fixed broken burger menu by adding link to specific Font Awesome version in `footer.html`


- In footer
    - ~~add basic site information page~~
    - add disclaimer page

- Check
    - ~~sitemap~~
    - ~~RSS feed~~

- Blog posts
    - date
        Using date pulled from filename, removing `date` section from Front Matter, appears redundant

    - ~~tags~~ Done. This is helpful: https://hyrtsi.github.io/2024/01/01/tags-links-jekyll.html
    - search?
    - make sure front page is only pulling in last 3 blogs

Header
    - ~~create data file, remove `tags` link~~ Done.

CSS/SASS
    - Added CSS and HTML to embed CV as PDF. Is this causing a flash? Getting a Firefox warning
    - Still haven't got sizing for CV embedded object correct.

Blog posts
    - Test adding a RMarkdown file.
    - Some problems here:
        - Need to use Knitr to output the markdown, because you need to process the chunks.
        - Added [Katex](https://katex.org/) typesetting library into header, which will render Latex.
        - Difference between `variant: markdown_github` and `variant: gfm`:
            - `variant: gfm` doesn't change Latex output
            - `variant: markdown_github` gives a `[WARNING] Deprecated` and the Latex is adjusted for the web, which is bad.

Front matter:
```
---
layout: post
title: Juselius Cointegrated Handbook
output:
md_document:
    variant: markdown_github
    preserve_yaml: true
katex: true    
---
```

- Add proper text for site description in bottom right.

- Add details to site page for Latex rendering.

# Exporting Rmd file from RStudio

- Backticks being added to Latex?

- Code blocks should be changed to:
```
{% highlight ruby %}
def foo
  puts 'foo'
end
{% endhighlight %}
```

# To Do

- Check domain set up
    - HTTPS on Github enabled
    - Currently getting `PR_CONNECT_RESET_ERROR` error
- Check email re-direct
- Set up Favicon.ico
- ~~Add links for supervisors~~
- ~~Add link for Adum/These.fr~~
- Update URL details in `_config.yml`
- Fix Faraday error, see here: https://stackoverflow.com/questions/72498899/deploy-github-pages-with-jekyll
- Update conferences with CFE
- Do I need to keep `_includes`:
    - `disqus_comments.html`
    - `google-analytics.html`
    - These are just going to create another thing for me to do!
- Do I need to add disclaimer/copyright? I don't think so.
- Update CV, remove photo, remove personal details.
