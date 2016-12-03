library(rvest)

#Given Writer, Get All Novel Pages

#Given Novel Pages, scrape synopses

#Given Novel Pages, scrape excerpt


test = read_html("http://nanowrimo.org/participants/nicaless/novels/a-mystery-in-the-kingdom-of-aermon")
test2 = html_nodes(test, "div", xpath = "//div[@id = 'novel_synopses']")
html_text(test2)

test = read_html("http://nanowrimo.org/participants/nicaless/novels/a-mystery-in-the-kingdom-of-aermon")
test2 = html_nodes(test, "div", xpath = "//div[@id = 'novel_excerpt']")
html_text(test2)
