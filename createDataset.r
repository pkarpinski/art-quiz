library('rvest')
library('data.table')

if (!file.exists('authorLinks.rds')) {
  fieldsPage <- read_html('https://www.wikiart.org/en/artists-by-field')
  
  fieldsLinks <- html_text(html_nodes(fieldsPage, xpath = '//ul[@class="dictionaries-list"]/li/a/@href'))
  fieldsLinks <- paste0('https://www.wikiart.org', fieldsLinks, '/text-list')
  
  authorLinks <- unlist(lapply(fieldsLinks, function(link) {
    return(html_text(html_nodes(read_html(link), xpath = '//main//ul/li/a/@href')))
  }))
  
  authorLinks <- paste0('https://www.wikiart.org', sub('/.*?/', '/en/', authorLinks))
  saveRDS(authorLinks, 'authorLinks.rds')
} else {
  authorLinks <- readRDS('authorLinks.rds')
}


getAuthorData <- function(link) {
  id <- strsplit(link, '/', fixed = T)[[1]]
  id <- id[length(id)]
  
  tryCatch({
    page <- read_html(link)
    infoNode <- html_node(page, xpath = '//*[@class="wiki-layout-artist-info"]')
    infoDetails <- html_node(infoNode, xpath = '//article')
    
    return(data.table(
      id = id,
      link = link,
      name = html_text(html_nodes(infoNode, xpath = '//*[@itemprop="name"]/@content')),
      additionalName = html_text(html_nodes(infoNode, xpath = '//*[@itemprop="additionalName"]')),
      image = html_text(html_nodes(infoNode, xpath = '//*[@itemprop="image"]/@src')),
      birthDate = html_text(html_nodes(infoDetails, xpath = '//*[@itemprop="birthDate"]')),
      birthPlace = html_text(html_nodes(infoDetails, xpath = '//*[@itemprop="birthPlace"]')),
      deathDate = html_text(html_nodes(infoDetails, xpath = '//*[@itemprop="deathDate"]')),
      deathPlace = html_text(html_nodes(infoDetails, xpath = '//*[@itemprop="deathPlace"]')),
      nationality = list(html_text(html_nodes(infoDetails, xpath = '//*[@itemprop="nationality"]'))),
      activeYears = trimws(html_text(html_nodes(infoDetails, xpath = '//*[text() = "Active Years:"]//following-sibling::text()'))),
      artMovement = list(html_text(html_nodes(infoDetails, xpath = '//*[text() = "Art Movement:"]//following-sibling::span/a'))),
      paintingSchool = list(html_text(html_nodes(infoDetails, xpath = '//*[text() = "Painting School:"]//following-sibling::span/a'))),
      genre = list(html_text(html_nodes(infoDetails, xpath = '//*[text() = "Genre:"]//following-sibling::span/a'))),
      field = list(html_text(html_nodes(infoDetails, xpath = '//*[text() = "Field:"]//following-sibling::span/a'))),
      influencedBy = list(html_text(html_nodes(infoDetails, xpath = '//*[text() = "Influenced by:"]//following-sibling::a'))),
      influencedOn = list(html_text(html_nodes(infoDetails, xpath = '//*[text() = "Influenced on:"]//following-sibling::a'))),
      teachers = list(html_text(html_nodes(infoDetails, xpath = '//*[text() = "Teachers:"]//following-sibling::a'))),
      pupils = list(html_text(html_nodes(infoDetails, xpath = '//*[text() = "Pupils:"]//following-sibling::a'))),
      friendsAndCoworkers = list(html_text(html_nodes(infoDetails, xpath = '//*[text() = "Friends and Co-workers:"]//following-sibling::a'))),
      familyAndRelatives = list(html_text(html_nodes(infoDetails, xpath = '//*[text() = "Family and Relatives:"]//following-sibling::a'))),
      artInstitution = list(html_text(html_nodes(infoDetails, xpath = '//*[text() = "Art institution:"]//following-sibling::a'))),
      wikipedia = html_text(html_node(infoDetails, xpath = '//*[text() = "Wikipedia:"]//following-sibling::*/@href')),
      raw = html_text(infoDetails)
    ))
  }, error = function(e) {
    return(data.table(
      id = id,
      name = "ERROR"
    ))
  })
}

# res <- rbindlist(lapply(authorLinks, getAuthorData), fill = T)

# saveRDS(res, 'authorData.rds')

getArtLinks <- function(authorLink) {
  authorId <- strsplit(authorLink, '/', fixed = T)[[1]]
  authorId <- authorId[length(authorId)]
  
  tryCatch({
    page <- read_html(paste0(authorLink, '/all-works/text-list'))
    urls <- html_text(html_nodes(page, xpath = '//*[@class="view-all-works"]//li/a/@href'))
    artId <- tstrsplit(urls, '/', fixed = T)[[4]]
    urls <- paste0('https://www.wikiart.org', sub('/.*?/', '/en/', urls))
    
    return(data.table(
      artId = artId,
      authorId = authorId,
      url = urls
    ))
  }, error = function(e) {
    return(data.table(
      authorId = authorId,
      url = "ERROR"
    ))
  })
}

# res2 <- rbindlist(lapply(authorLinks, getArtLinks), fill = T)
# 
# saveRDS(res2, 'artLinks.rds')
