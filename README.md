# Quick Search
The purpose of this package is to facilitate searching things on the web directly from Emacs (without needing to resort to eww).
You can customize specific search engines for specific modes (and even set engines globally).

## Demonstration:
https://github.com/lima-limon-inc/Emacs-Quick-Search/assets/65001595/bfb221ca-06f8-4d91-9137-bf6155240305

## Installation
Simply copy `quick-search.el`'s source code into your init.el. After that, you can add the configuration code.

## Configuration
There are 2 function that you need to set up in order to use `quick-search`

### Preferred browser
`quick-search/set-preferred-browser` configures your browser of choice.
It has two arguments:
1. Your preferred browser's name
2. Any command line arguments you'd like to pass

Examples:
- `(quick-search/set-preferred-browser "firefox" "")`
- `(quick-search/set-preferred-browser "chromium" "--incognito")`

### Search engines
`quick-search/add-search-engine` adds a search engine to the search engine list.
It has 3 arguments:
1. The name of the search engine
2. Search engine link (normally found on the URL of websites after searching for something)
3. A quoted list of modes you'd like the search engine to be enabled in. Write `nil` if you'd like to enable it globally.
Examples:
- `(quick-search/add-search-engine "Google" "https://google.com/search?q=" nil)`
- `(quick-search/add-search-engine "YouTube" "https://www.youtube.com/results?search_query=" '(elisp-mode js-mode rust-mode))`
- `(quick-search/add-search-engine "C plus plus" "https://cplusplus.com/search.do?q=" '(c++-mode c-mode))`

## Usage
Simply call the `quick-search` function after having it configured (you need to have a selected browser and at least 1 search engine)

## Example configuration
```elisp
; This sets firefox as the default browser with no extra arguments
(quick-search/set-preferred-browser "firefox" "")

; Adds google search engine to be enabled in all the modes
(quick-search/add-search-engine "Google" "https://google.com/search?q=" nil)

; Same thing as above but for Wikipedia
(quick-search/add-search-engine "Wikipedia" "https://en.wikipedia.org/w/index.php?search=" nil)

; Enables the python documentation for Python mode
(quick-search/add-search-engine "Python" "https://docs.python.org/3/search.html?q=" '(python-mode))

; Enables C Plus Plus website for both C and C++ mode
(quick-search/add-search-engine "C plus plus" "https://cplusplus.com/search.do?q=" '(c++-mode c-mode))
```

You can even add the function to a keybinding.
```elisp
(global-set-key "\C-c\C-s" 'quick-search)
```

evil-leader users can also do the following:
```elisp
(evil-leader/set-key "s" 'quick-search)
```
## Credits
- Thanks to @heemayl for the original foundation. Check out their [website](https://heemayl.net/)
- Thanks to Trey Jackson for [this](https://stackoverflow.com/a/1511827) stackoverflow answer. Check out their [website](http://trey-jackson.blogspot.com/). If anyone can find his GitHub profile, please make a PR.

## Disclaimer
I am not affiliated with any of the search engines/browsers mentioned. Other options are available 
