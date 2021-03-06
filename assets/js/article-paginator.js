(function () {
  var offset = GlobalConfig.offset;

  var showElement = (e) => {
    e.style.display = 'block';
  };

  var showInlineElement = (e) => {
    e.style.display = 'inline';
  };

  var hideElement = (e) => {
    e.style.display = 'none';
  };

  var forEach = (arrLike, fn) => {
    Array.prototype.forEach.call(arrLike, fn);
  };

  var parseTagAndPage = (hash) => {
    // #PAGE
    var match = hash.match(/^#(\d+)\/?$/);
    if (match) {
      return { tag: null, page: match[1] ? parseInt(match[1]) : 0 };
    }
    // #TAG/PAGE
    match = hash.match(/^#([^\/]+)(\/(\d*)\/?)?$/);
    if (match) return {
      tag: match[1] ? decodeURI(match[1]) : null,
      page: match[3] ? parseInt(match[3]) : 0
    };
    return { tag: null, page: 0 };
  };

  var showPage = (articles, page) => {
    var startIndex = offset * page;
    Array.prototype.slice.call(articles, startIndex, startIndex + offset)
         .forEach(showElement);
  };

  var urlFor = (tag, page) => {
    if (tag) return `#${tag}/${page}`;
    else return `#${page}`;
  }

  var setNavigation = (articles, tag, page) => {
    if (page > 0) {
      document.getElementById('prev-page-link')
              .setAttribute('href', urlFor(tag, page - 1));
      showElement(document.getElementById('prev-page'));
    } else {
      hideElement(document.getElementById('prev-page'));
    }
    if (offset * (page + 1) < articles.length) {
      document.getElementById('next-page-link')
              .setAttribute('href', urlFor(tag, page + 1));
      showElement(document.getElementById('next-page'));
    } else {
      hideElement(document.getElementById('next-page'));
    }
  };

  var filterTags = () => {
    forEach(document.getElementsByTagName('article'), hideElement);
    hideElement(document.getElementById('tag-unknown'));
    forEach(document.getElementsByClassName('show-only-tag-exists'), hideElement);

    var loc = parseTagAndPage(location.hash);
    var currentTag = loc.tag;
    var currentPage = loc.page;
    var articles;

    if (currentTag) {
      articles = document.getElementsByClassName(`tag-${currentTag}`);
      forEach(
        document.getElementsByClassName('tag-name'),
        (e) => e.innerText = currentTag
      );
      forEach(
        document.getElementsByClassName('show-only-tag-exists'),
        showInlineElement
      );
    } else {
      articles = document.getElementsByTagName('article');
      forEach(
        document.getElementsByClassName('show-only-tag-exists'),
        hideElement
      );
    }

    if (articles.length > 0) {
      showPage(articles, currentPage);
      setNavigation(articles, currentTag, currentPage);
    } else if (currentTag) {
      showElement(document.getElementById('tag-unknown'));
    }
  }

  if (document.readyState !== 'loading') {
    filterTags();
  } else {
    document.addEventListener('DOMContentLoaded', () => filterTags());
  }

  window.addEventListener('hashchange', () => {
    filterTags();
    window.scrollTo(0, 0);
  });
})();
