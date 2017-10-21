// <script data-cfasync="true" src="/js/run_prettify.js"></script>
// <script data-cfasync="true" src="/js/jquery.js"></script>
// <script> // Articles.js
// Pagination and the article scroller in the side
jQuery(function(){
    var pagination = function(articles, perPage) {
        // Prepare initial variables and setup
        jQuery("#footer").before("<div id='pagination'></div>");
        var eSidebar = jQuery("#articles-sidebar"),
            eSidebarDown = jQuery("#articles-sidebar-item-bottom"),
            ePagination = jQuery("#pagination"),
            hashPage = null,
            pages = Math.floor(articles.length / perPage);
        if (articles.length % perPage !== 0) {
            pages++;
        }
        for(var i = 1; i <= pages; i++) {
            ePagination.append("<span class='pagination-page'>" + i + "</span>");
        }

        // Set the active page in the sidebar
        var setActivePost = function() {
            if(jQuery(window).scrollTop() + jQuery(window).height() > jQuery(document).height() - 20) {
                jQuery("#articles-sidebar .articles-sidebar-item").removeClass("active");
                jQuery("#articles-sidebar-item-bottom").addClass("active");
            } else {
                jQuery("#articles-sidebar-item-bottom").removeClass("active");
                if (jQuery(window).scrollTop() < 51) {
                    jQuery(jQuery("#articles-sidebar .articles-sidebar-item")[0]).addClass("active");
                } else {
                    var children = jQuery(".article.show").children(".article-header").children("a").children("h1");
                    for (var i = 0; i < children.length; i++) {
                        var child = children[i];
                        if ((jQuery(child).position().top - 27) < jQuery(window).scrollTop()) {
                            jQuery("#articles-sidebar .articles-sidebar-item").removeClass("active");
                            jQuery(jQuery("#articles-sidebar .articles-sidebar-item")[i]).addClass("active");
                        }
                    }
                }
            }
        };

        // Activate a pagination element
        var activatePage = function(elem) {
            var page = jQuery(elem).html(),
                offset = (page * perPage) - perPage;
            jQuery("#articles-sidebar .articles-sidebar-item").remove(); // clear the sidebar
            for (var i = 0; i < articles.length; i++) {
                if (i < offset || i >= offset + perPage) {
                    jQuery(articles[i]).removeClass("show last").addClass("hidden");
                } else {
                    var eArticleHeader = jQuery(jQuery(".article-header h1", articles[i])[0]);
                    eSidebarDown.before("<div class='articles-sidebar-item'>" + eArticleHeader.html() + "</div>"); // this is for the sidebar
                    jQuery(articles[i]).removeClass("hidden last").addClass("show");
                    if (i == offset + perPage - 1 || i == articles.length - 1) {
                        jQuery(articles[i]).addClass("last");
                    }
                }
            }
            jQuery("#pagination .pagination-page").removeClass("active");
            jQuery(elem).addClass("active");
            jQuery("html, body").animate({ scrollTop: 0 }, 200);
            window.location.hash = "!/" + page;
            setActivePost();
        };
        jQuery("#pagination").on("click", ".pagination-page", function() {
            activatePage(this);
        });

        // Handle the hash, activating pagination elements etc
        var handleHash = function(hash) {
            var urlHash = window.location.hash,
                newHashPage = null;
            if (urlHash.substr(3) > 0 && urlHash.substr(3) <= pages) {
                newHashPage = urlHash.substr(3) - 1;
            }
            if (newHashPage !== null && hashPage != newHashPage) {
                hashPage = newHashPage;
                activatePage(jQuery("#pagination .pagination-page")[hashPage]);
            } else if (hashPage === null) {
                activatePage(jQuery("#pagination .pagination-page")[0]);
            }
        };

        // Check for hash changes
        var hashCheck = function() {
            setTimeout(function() {
                handleHash();
                hashCheck();
            }, 500);
        };
        handleHash();
        hashCheck();

        jQuery(window).scroll(function() {
            setActivePost();
        });
        setActivePost();
        jQuery("#articles-sidebar-item-bottom").on("click", function() {
            if (jQuery("#articles-sidebar-item-bottom").hasClass("active")) {
                jQuery("html, body").animate({ scrollTop: 0 }, 200);
            } else {
                jQuery("html, body").animate({ scrollTop: jQuery(document).height() }, 200);
            }
        });
        jQuery("#articles-sidebar").on("click", ".articles-sidebar-item", function() {
            var index = jQuery("#articles-sidebar .articles-sidebar-item").index(this);
            jQuery("html, body").animate({ scrollTop: jQuery(jQuery(".article.show")[index]).position().top }, 200);
        });
    };

    var Articles = jQuery("#content .article");
    var ArticlesPerPage = 3;
    if (Articles.length > 0) {
        pagination(Articles, ArticlesPerPage);
    }
    jQuery("#articles-sidebar").removeClass("hidden");

});
// </script>