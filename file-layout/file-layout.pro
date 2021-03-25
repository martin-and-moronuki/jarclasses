---

CSS for the website is in the #styleDir{style} directory.

Most of the content is in these directories:

    Menus are under #contentDir{menus}.

    Miscellaneous blog posts are under #contentDir{posts}.

There are also a few additional pages:

    #-page:
        The page at #url{/} is called the home page.
        Its text is written in #input{home/home.pro}.
        The HTML is written to #output{home/home.html} and ends up at #deploy{index.html}.
    #:

    #-page:
        The front page for the menus section is #input{menus/menus.pro}.
        Its HTML is written to #output{menus/menus.html}.
        Its URL is #url{/menus}, and it ends up at #deploy{menus/index.html}.
    #:

    #-page:
        The page for displaying the list of tags is #input{tags/tags.pro}.
        Its HTML is written to #output{tags/tags.html}.
        Its URL is #url{/tags}, and it ends up at #deploy{tags/index.html}.
    #:
