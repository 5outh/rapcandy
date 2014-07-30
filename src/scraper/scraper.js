var express = require('express');
var fs = require('fs');
var request = require('request');
var cheerio = require('cheerio');
var _       = require('underscore');

var removeBogusChars = function(str){
  return str.replace(/[^\w\s]/gi, '');
}

var scrape = function(){

  // Let's scrape Anchorman 2
  url = 'http://www.mldb.org/artist-102-eminem.html';

  request(url, function(error, response, html){
    if(!error){
      var $ = cheerio.load(html);

      var songs = [];

      $('a').each(function(i, elem) {
        var href = $(this).attr('href');
        
        if(href.match(/song-/)){
          songs.push({
            name: removeBogusChars( $(this).text().split(' ').join('') ),
            href: href
          });
        }
      });

      _(songs).map(function(song){
        request('http://www.mldb.org/' + song.href, function(err, res, _html){
          var $$ = cheerio.load(_html);

          fs.writeFileSync("./lyrics/" + song.name + ".txt", $$('.songtext').text());
        });
        console.log(song);
      });
    }
  });
};

scrape();