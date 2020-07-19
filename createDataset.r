library('rvest')

page <- read_html('https://www.wikiart.org/en/leonardo-da-vinci')

birthDate <- html_text(html_node(page, xpath = '//span[@itemprop="birthDate"]'))