


<!DOCTYPE html>
<html>
  <head prefix="og: http://ogp.me/ns# fb: http://ogp.me/ns/fb# githubog: http://ogp.me/ns/fb/githubog#">
    <meta charset='utf-8'>
    <meta http-equiv="X-UA-Compatible" content="IE=edge">
        <title>fastclick/lib/fastclick.js at master · ftlabs/fastclick</title>
    <link rel="search" type="application/opensearchdescription+xml" href="/opensearch.xml" title="GitHub" />
    <link rel="fluid-icon" href="https://github.com/fluidicon.png" title="GitHub" />
    <link rel="apple-touch-icon" sizes="57x57" href="/apple-touch-icon-114.png" />
    <link rel="apple-touch-icon" sizes="114x114" href="/apple-touch-icon-114.png" />
    <link rel="apple-touch-icon" sizes="72x72" href="/apple-touch-icon-144.png" />
    <link rel="apple-touch-icon" sizes="144x144" href="/apple-touch-icon-144.png" />
    <link rel="logo" type="image/svg" href="https://github-media-downloads.s3.amazonaws.com/github-logo.svg" />
    <meta property="og:image" content="https://github.global.ssl.fastly.net/images/modules/logos_page/Octocat.png">
    <meta name="hostname" content="github-fe128-cp1-prd.iad.github.net">
    <meta name="ruby" content="ruby 1.9.3p194-tcs-github-tcmalloc (e1c0c3f392) [x86_64-linux]">
    <link rel="assets" href="https://github.global.ssl.fastly.net/">
    <link rel="conduit-xhr" href="https://ghconduit.com:25035/">
    <link rel="xhr-socket" href="/_sockets" />
    


    <meta name="msapplication-TileImage" content="/windows-tile.png" />
    <meta name="msapplication-TileColor" content="#ffffff" />
    <meta name="selected-link" value="repo_source" data-pjax-transient />
    <meta content="collector.githubapp.com" name="octolytics-host" /><meta content="collector-cdn.github.com" name="octolytics-script-host" /><meta content="github" name="octolytics-app-id" /><meta content="8110C7B5:7CE8:5119DF9:529899A7" name="octolytics-dimension-request_id" /><meta content="2615349" name="octolytics-actor-id" /><meta content="mangeh" name="octolytics-actor-login" /><meta content="8e7125f60f99ca04ed68d549e91239d6298613ebca9e463a771b532f69ddc93b" name="octolytics-actor-hash" />
    

    
    
    <link rel="icon" type="image/x-icon" href="/favicon.ico" />

    <meta content="authenticity_token" name="csrf-param" />
<meta content="WS08n5aUshyRo5rRnkn/OoMhYearVjQWM+zLaMJOj0Q=" name="csrf-token" />

    <link href="https://github.global.ssl.fastly.net/assets/github-0f5611c27a5a2a6928dc6e99d63581265b963e34.css" media="all" rel="stylesheet" type="text/css" />
    <link href="https://github.global.ssl.fastly.net/assets/github2-6102d7944435d804d870f38bf20f1e16fe40a4d0.css" media="all" rel="stylesheet" type="text/css" />
    

    

      <script src="https://github.global.ssl.fastly.net/assets/frameworks-f78f9db32da9343ba16e477c7b54aa5b971fecbd.js" type="text/javascript"></script>
      <script src="https://github.global.ssl.fastly.net/assets/github-0027dfa2fe63e83319cafa77f57322ac81018255.js" type="text/javascript"></script>
      
      <meta http-equiv="x-pjax-version" content="0741c61298bb1b3e07c83e272155021d">

        <link data-pjax-transient rel='permalink' href='/ftlabs/fastclick/blob/31e2fb286406ed553904c14ca4b38abef7f94226/lib/fastclick.js'>
  <meta property="og:title" content="fastclick"/>
  <meta property="og:type" content="githubog:gitrepository"/>
  <meta property="og:url" content="https://github.com/ftlabs/fastclick"/>
  <meta property="og:image" content="https://github.global.ssl.fastly.net/images/gravatars/gravatar-user-420.png"/>
  <meta property="og:site_name" content="GitHub"/>
  <meta property="og:description" content="fastclick - Polyfill to remove click delays on browsers with touch UIs"/>

  <meta name="description" content="fastclick - Polyfill to remove click delays on browsers with touch UIs" />

  <meta content="1432696" name="octolytics-dimension-user_id" /><meta content="ftlabs" name="octolytics-dimension-user_login" /><meta content="3428345" name="octolytics-dimension-repository_id" /><meta content="ftlabs/fastclick" name="octolytics-dimension-repository_nwo" /><meta content="true" name="octolytics-dimension-repository_public" /><meta content="false" name="octolytics-dimension-repository_is_fork" /><meta content="3428345" name="octolytics-dimension-repository_network_root_id" /><meta content="ftlabs/fastclick" name="octolytics-dimension-repository_network_root_nwo" />
  <link href="https://github.com/ftlabs/fastclick/commits/master.atom" rel="alternate" title="Recent Commits to fastclick:master" type="application/atom+xml" />

  </head>


  <body class="logged_in  env-production windows vis-public page-blob">
    <div class="wrapper">
      
      
      
      


      <div class="header header-logged-in true">
  <div class="container clearfix">

    <a class="header-logo-invertocat" href="https://github.com/organizations/MercuryGroup">
  <span class="mega-octicon octicon-mark-github"></span>
</a>

    
    <a href="/notifications" class="notification-indicator tooltipped downwards" data-gotokey="n" title="You have unread notifications">
        <span class="mail-status unread"></span>
</a>

      <div class="command-bar js-command-bar  in-repository">
          <form accept-charset="UTF-8" action="/search" class="command-bar-form" id="top_search_form" method="get">

<input type="text" data-hotkey="/ s" name="q" id="js-command-bar-field" placeholder="Search or type a command" tabindex="1" autocapitalize="off"
    
    data-username="mangeh"
      data-repo="ftlabs/fastclick"
      data-branch="master"
      data-sha="f387efe76d10f2d7c9d958f0654491b82405fa6b"
  >

    <input type="hidden" name="nwo" value="ftlabs/fastclick" />

    <div class="select-menu js-menu-container js-select-menu search-context-select-menu">
      <span class="minibutton select-menu-button js-menu-target">
        <span class="js-select-button">This repository</span>
      </span>

      <div class="select-menu-modal-holder js-menu-content js-navigation-container">
        <div class="select-menu-modal">

          <div class="select-menu-item js-navigation-item js-this-repository-navigation-item selected">
            <span class="select-menu-item-icon octicon octicon-check"></span>
            <input type="radio" class="js-search-this-repository" name="search_target" value="repository" checked="checked" />
            <div class="select-menu-item-text js-select-button-text">This repository</div>
          </div> <!-- /.select-menu-item -->

          <div class="select-menu-item js-navigation-item js-all-repositories-navigation-item">
            <span class="select-menu-item-icon octicon octicon-check"></span>
            <input type="radio" name="search_target" value="global" />
            <div class="select-menu-item-text js-select-button-text">All repositories</div>
          </div> <!-- /.select-menu-item -->

        </div>
      </div>
    </div>

  <span class="octicon help tooltipped downwards" title="Show command bar help">
    <span class="octicon octicon-question"></span>
  </span>


  <input type="hidden" name="ref" value="cmdform">

</form>
        <ul class="top-nav">
          <li class="explore"><a href="/explore">Explore</a></li>
            <li><a href="https://gist.github.com">Gist</a></li>
            <li><a href="/blog">Blog</a></li>
          <li><a href="https://help.github.com">Help</a></li>
        </ul>
      </div>

    


  <ul id="user-links">
    <li>
      <a href="/mangeh" class="name">
        <img height="20" src="https://2.gravatar.com/avatar/346a4f93475570c35f88837bdbbae97b?d=https%3A%2F%2Fidenticons.github.com%2F38ad4c658f6e519ed18d370fcf1a58e2.png&amp;r=x&amp;s=140" width="20" /> mangeh
      </a>
    </li>

      <li>
        <a href="/new" id="new_repo" class="tooltipped downwards" title="Create a new repo" aria-label="Create a new repo">
          <span class="octicon octicon-repo-create"></span>
        </a>
      </li>

      <li>
        <a href="/settings/profile" id="account_settings"
          class="tooltipped downwards"
          aria-label="Account settings "
          title="Account settings ">
          <span class="octicon octicon-tools"></span>
        </a>
      </li>
      <li>
        <a class="tooltipped downwards" href="/logout" data-method="post" id="logout" title="Sign out" aria-label="Sign out">
          <span class="octicon octicon-log-out"></span>
        </a>
      </li>

  </ul>

<div class="js-new-dropdown-contents hidden">
  

<ul class="dropdown-menu">
  <li>
    <a href="/new"><span class="octicon octicon-repo-create"></span> New repository</a>
  </li>
  <li>
    <a href="/organizations/new"><span class="octicon octicon-organization"></span> New organization</a>
  </li>



    <li class="section-title">
      <span title="ftlabs/fastclick">This repository</span>
    </li>
      <li>
        <a href="/ftlabs/fastclick/issues/new"><span class="octicon octicon-issue-opened"></span> New issue</a>
      </li>
</ul>

</div>


    
  </div>
</div>

      

      




          <div class="site" itemscope itemtype="http://schema.org/WebPage">
    
    <div class="pagehead repohead instapaper_ignore readability-menu">
      <div class="container">
        

<ul class="pagehead-actions">

    <li class="subscription">
      <form accept-charset="UTF-8" action="/notifications/subscribe" class="js-social-container" data-autosubmit="true" data-remote="true" method="post"><div style="margin:0;padding:0;display:inline"><input name="authenticity_token" type="hidden" value="WS08n5aUshyRo5rRnkn/OoMhYearVjQWM+zLaMJOj0Q=" /></div>  <input id="repository_id" name="repository_id" type="hidden" value="3428345" />

    <div class="select-menu js-menu-container js-select-menu">
      <a class="social-count js-social-count" href="/ftlabs/fastclick/watchers">
        268
      </a>
      <span class="minibutton select-menu-button with-count js-menu-target" role="button" tabindex="0">
        <span class="js-select-button">
          <span class="octicon octicon-eye-watch"></span>
          Watch
        </span>
      </span>

      <div class="select-menu-modal-holder">
        <div class="select-menu-modal subscription-menu-modal js-menu-content">
          <div class="select-menu-header">
            <span class="select-menu-title">Notification status</span>
            <span class="octicon octicon-remove-close js-menu-close"></span>
          </div> <!-- /.select-menu-header -->

          <div class="select-menu-list js-navigation-container" role="menu">

            <div class="select-menu-item js-navigation-item selected" role="menuitem" tabindex="0">
              <span class="select-menu-item-icon octicon octicon-check"></span>
              <div class="select-menu-item-text">
                <input checked="checked" id="do_included" name="do" type="radio" value="included" />
                <h4>Not watching</h4>
                <span class="description">You only receive notifications for conversations in which you participate or are @mentioned.</span>
                <span class="js-select-button-text hidden-select-button-text">
                  <span class="octicon octicon-eye-watch"></span>
                  Watch
                </span>
              </div>
            </div> <!-- /.select-menu-item -->

            <div class="select-menu-item js-navigation-item " role="menuitem" tabindex="0">
              <span class="select-menu-item-icon octicon octicon octicon-check"></span>
              <div class="select-menu-item-text">
                <input id="do_subscribed" name="do" type="radio" value="subscribed" />
                <h4>Watching</h4>
                <span class="description">You receive notifications for all conversations in this repository.</span>
                <span class="js-select-button-text hidden-select-button-text">
                  <span class="octicon octicon-eye-unwatch"></span>
                  Unwatch
                </span>
              </div>
            </div> <!-- /.select-menu-item -->

            <div class="select-menu-item js-navigation-item " role="menuitem" tabindex="0">
              <span class="select-menu-item-icon octicon octicon-check"></span>
              <div class="select-menu-item-text">
                <input id="do_ignore" name="do" type="radio" value="ignore" />
                <h4>Ignoring</h4>
                <span class="description">You do not receive any notifications for conversations in this repository.</span>
                <span class="js-select-button-text hidden-select-button-text">
                  <span class="octicon octicon-mute"></span>
                  Stop ignoring
                </span>
              </div>
            </div> <!-- /.select-menu-item -->

          </div> <!-- /.select-menu-list -->

        </div> <!-- /.select-menu-modal -->
      </div> <!-- /.select-menu-modal-holder -->
    </div> <!-- /.select-menu -->

</form>
    </li>

  <li>
  

  <div class="js-toggler-container js-social-container starring-container ">
    <a href="/ftlabs/fastclick/unstar"
      class="minibutton with-count js-toggler-target star-button starred upwards"
      title="Unstar this repository" data-remote="true" data-method="post" rel="nofollow">
      <span class="octicon octicon-star-delete"></span><span class="text">Unstar</span>
    </a>

    <a href="/ftlabs/fastclick/star"
      class="minibutton with-count js-toggler-target star-button unstarred upwards"
      title="Star this repository" data-remote="true" data-method="post" rel="nofollow">
      <span class="octicon octicon-star"></span><span class="text">Star</span>
    </a>

      <a class="social-count js-social-count" href="/ftlabs/fastclick/stargazers">
        4,616
      </a>
  </div>

  </li>


        <li>
          <a href="/ftlabs/fastclick/fork" class="minibutton with-count js-toggler-target fork-button lighter upwards" title="Fork this repo" rel="facebox nofollow">
            <span class="octicon octicon-git-branch-create"></span><span class="text">Fork</span>
          </a>
          <a href="/ftlabs/fastclick/network" class="social-count">527</a>
        </li>


</ul>

        <h1 itemscope itemtype="http://data-vocabulary.org/Breadcrumb" class="entry-title public">
          <span class="repo-label"><span>public</span></span>
          <span class="mega-octicon octicon-repo"></span>
          <span class="author">
            <a href="/ftlabs" class="url fn" itemprop="url" rel="author"><span itemprop="title">ftlabs</span></a>
          </span>
          <span class="repohead-name-divider">/</span>
          <strong><a href="/ftlabs/fastclick" class="js-current-repository js-repo-home-link">fastclick</a></strong>

          <span class="page-context-loader">
            <img alt="Octocat-spinner-32" height="16" src="https://github.global.ssl.fastly.net/images/spinners/octocat-spinner-32.gif" width="16" />
          </span>

        </h1>
      </div><!-- /.container -->
    </div><!-- /.repohead -->

    

    <div class="container">

      <div class="repository-with-sidebar repo-container  ">

        <div class="repository-sidebar">
            

<div class="sunken-menu vertical-right repo-nav js-repo-nav js-repository-container-pjax js-octicon-loaders">
  <div class="sunken-menu-contents">
    <ul class="sunken-menu-group">
      <li class="tooltipped leftwards" title="Code">
        <a href="/ftlabs/fastclick" aria-label="Code" class="selected js-selected-navigation-item sunken-menu-item" data-gotokey="c" data-pjax="true" data-selected-links="repo_source repo_downloads repo_commits repo_tags repo_branches /ftlabs/fastclick">
          <span class="octicon octicon-code"></span> <span class="full-word">Code</span>
          <img alt="Octocat-spinner-32" class="mini-loader" height="16" src="https://github.global.ssl.fastly.net/images/spinners/octocat-spinner-32.gif" width="16" />
</a>      </li>

        <li class="tooltipped leftwards" title="Issues">
          <a href="/ftlabs/fastclick/issues" aria-label="Issues" class="js-selected-navigation-item sunken-menu-item js-disable-pjax" data-gotokey="i" data-selected-links="repo_issues /ftlabs/fastclick/issues">
            <span class="octicon octicon-issue-opened"></span> <span class="full-word">Issues</span>
            <span class='counter'>46</span>
            <img alt="Octocat-spinner-32" class="mini-loader" height="16" src="https://github.global.ssl.fastly.net/images/spinners/octocat-spinner-32.gif" width="16" />
</a>        </li>

      <li class="tooltipped leftwards" title="Pull Requests"><a href="/ftlabs/fastclick/pulls" aria-label="Pull Requests" class="js-selected-navigation-item sunken-menu-item js-disable-pjax" data-gotokey="p" data-selected-links="repo_pulls /ftlabs/fastclick/pulls">
            <span class="octicon octicon-git-pull-request"></span> <span class="full-word">Pull Requests</span>
            <span class='counter'>6</span>
            <img alt="Octocat-spinner-32" class="mini-loader" height="16" src="https://github.global.ssl.fastly.net/images/spinners/octocat-spinner-32.gif" width="16" />
</a>      </li>


    </ul>
    <div class="sunken-menu-separator"></div>
    <ul class="sunken-menu-group">

      <li class="tooltipped leftwards" title="Pulse">
        <a href="/ftlabs/fastclick/pulse" aria-label="Pulse" class="js-selected-navigation-item sunken-menu-item" data-pjax="true" data-selected-links="pulse /ftlabs/fastclick/pulse">
          <span class="octicon octicon-pulse"></span> <span class="full-word">Pulse</span>
          <img alt="Octocat-spinner-32" class="mini-loader" height="16" src="https://github.global.ssl.fastly.net/images/spinners/octocat-spinner-32.gif" width="16" />
</a>      </li>

      <li class="tooltipped leftwards" title="Graphs">
        <a href="/ftlabs/fastclick/graphs" aria-label="Graphs" class="js-selected-navigation-item sunken-menu-item" data-pjax="true" data-selected-links="repo_graphs repo_contributors /ftlabs/fastclick/graphs">
          <span class="octicon octicon-graph"></span> <span class="full-word">Graphs</span>
          <img alt="Octocat-spinner-32" class="mini-loader" height="16" src="https://github.global.ssl.fastly.net/images/spinners/octocat-spinner-32.gif" width="16" />
</a>      </li>

      <li class="tooltipped leftwards" title="Network">
        <a href="/ftlabs/fastclick/network" aria-label="Network" class="js-selected-navigation-item sunken-menu-item js-disable-pjax" data-selected-links="repo_network /ftlabs/fastclick/network">
          <span class="octicon octicon-git-branch"></span> <span class="full-word">Network</span>
          <img alt="Octocat-spinner-32" class="mini-loader" height="16" src="https://github.global.ssl.fastly.net/images/spinners/octocat-spinner-32.gif" width="16" />
</a>      </li>
    </ul>


  </div>
</div>

            <div class="only-with-full-nav">
              

  

<div class="clone-url open"
  data-protocol-type="http"
  data-url="/users/set_protocol?protocol_selector=http&amp;protocol_type=clone">
  <h3><strong>HTTPS</strong> clone URL</h3>
  <div class="clone-url-box">
    <input type="text" class="clone js-url-field"
           value="https://github.com/ftlabs/fastclick.git" readonly="readonly">

    <span class="js-zeroclipboard url-box-clippy minibutton zeroclipboard-button" data-clipboard-text="https://github.com/ftlabs/fastclick.git" data-copied-hint="copied!" title="copy to clipboard"><span class="octicon octicon-clippy"></span></span>
  </div>
</div>

  

<div class="clone-url "
  data-protocol-type="ssh"
  data-url="/users/set_protocol?protocol_selector=ssh&amp;protocol_type=clone">
  <h3><strong>SSH</strong> clone URL</h3>
  <div class="clone-url-box">
    <input type="text" class="clone js-url-field"
           value="git@github.com:ftlabs/fastclick.git" readonly="readonly">

    <span class="js-zeroclipboard url-box-clippy minibutton zeroclipboard-button" data-clipboard-text="git@github.com:ftlabs/fastclick.git" data-copied-hint="copied!" title="copy to clipboard"><span class="octicon octicon-clippy"></span></span>
  </div>
</div>

  

<div class="clone-url "
  data-protocol-type="subversion"
  data-url="/users/set_protocol?protocol_selector=subversion&amp;protocol_type=clone">
  <h3><strong>Subversion</strong> checkout URL</h3>
  <div class="clone-url-box">
    <input type="text" class="clone js-url-field"
           value="https://github.com/ftlabs/fastclick" readonly="readonly">

    <span class="js-zeroclipboard url-box-clippy minibutton zeroclipboard-button" data-clipboard-text="https://github.com/ftlabs/fastclick" data-copied-hint="copied!" title="copy to clipboard"><span class="octicon octicon-clippy"></span></span>
  </div>
</div>


<p class="clone-options">You can clone with
      <a href="#" class="js-clone-selector" data-protocol="http">HTTPS</a>,
      <a href="#" class="js-clone-selector" data-protocol="ssh">SSH</a>,
      or <a href="#" class="js-clone-selector" data-protocol="subversion">Subversion</a>.
  <span class="octicon help tooltipped upwards" title="Get help on which URL is right for you.">
    <a href="https://help.github.com/articles/which-remote-url-should-i-use">
    <span class="octicon octicon-question"></span>
    </a>
  </span>
</p>


  <a href="github-windows://openRepo/https://github.com/ftlabs/fastclick" class="minibutton sidebar-button">
    <span class="octicon octicon-device-desktop"></span>
    Clone in Desktop
  </a>

              <a href="/ftlabs/fastclick/archive/master.zip"
                 class="minibutton sidebar-button"
                 title="Download this repository as a zip file"
                 rel="nofollow">
                <span class="octicon octicon-cloud-download"></span>
                Download ZIP
              </a>
            </div>
        </div><!-- /.repository-sidebar -->

        <div id="js-repo-pjax-container" class="repository-content context-loader-container" data-pjax-container>
          


<!-- blob contrib key: blob_contributors:v21:661b8f96da893494d316e3db9702521e -->

<p title="This is a placeholder element" class="js-history-link-replace hidden"></p>

<a href="/ftlabs/fastclick/find/master" data-pjax data-hotkey="t" class="js-show-file-finder" style="display:none">Show File Finder</a>

<div class="file-navigation">
  

<div class="select-menu js-menu-container js-select-menu" >
  <span class="minibutton select-menu-button js-menu-target" data-hotkey="w"
    data-master-branch="master"
    data-ref="master"
    role="button" aria-label="Switch branches or tags" tabindex="0">
    <span class="octicon octicon-git-branch"></span>
    <i>branch:</i>
    <span class="js-select-button">master</span>
  </span>

  <div class="select-menu-modal-holder js-menu-content js-navigation-container" data-pjax>

    <div class="select-menu-modal">
      <div class="select-menu-header">
        <span class="select-menu-title">Switch branches/tags</span>
        <span class="octicon octicon-remove-close js-menu-close"></span>
      </div> <!-- /.select-menu-header -->

      <div class="select-menu-filters">
        <div class="select-menu-text-filter">
          <input type="text" aria-label="Filter branches/tags" id="context-commitish-filter-field" class="js-filterable-field js-navigation-enable" placeholder="Filter branches/tags">
        </div>
        <div class="select-menu-tabs">
          <ul>
            <li class="select-menu-tab">
              <a href="#" data-tab-filter="branches" class="js-select-menu-tab">Branches</a>
            </li>
            <li class="select-menu-tab">
              <a href="#" data-tab-filter="tags" class="js-select-menu-tab">Tags</a>
            </li>
          </ul>
        </div><!-- /.select-menu-tabs -->
      </div><!-- /.select-menu-filters -->

      <div class="select-menu-list select-menu-tab-bucket js-select-menu-tab-bucket" data-tab-filter="branches">

        <div data-filterable-for="context-commitish-filter-field" data-filterable-type="substring">


            <div class="select-menu-item js-navigation-item ">
              <span class="select-menu-item-icon octicon octicon-check"></span>
              <a href="/ftlabs/fastclick/blob/chrome-prevent/lib/fastclick.js"
                 data-name="chrome-prevent"
                 data-skip-pjax="true"
                 rel="nofollow"
                 class="js-navigation-open select-menu-item-text js-select-button-text css-truncate-target"
                 title="chrome-prevent">chrome-prevent</a>
            </div> <!-- /.select-menu-item -->
            <div class="select-menu-item js-navigation-item ">
              <span class="select-menu-item-icon octicon octicon-check"></span>
              <a href="/ftlabs/fastclick/blob/gh-pages/lib/fastclick.js"
                 data-name="gh-pages"
                 data-skip-pjax="true"
                 rel="nofollow"
                 class="js-navigation-open select-menu-item-text js-select-button-text css-truncate-target"
                 title="gh-pages">gh-pages</a>
            </div> <!-- /.select-menu-item -->
            <div class="select-menu-item js-navigation-item selected">
              <span class="select-menu-item-icon octicon octicon-check"></span>
              <a href="/ftlabs/fastclick/blob/master/lib/fastclick.js"
                 data-name="master"
                 data-skip-pjax="true"
                 rel="nofollow"
                 class="js-navigation-open select-menu-item-text js-select-button-text css-truncate-target"
                 title="master">master</a>
            </div> <!-- /.select-menu-item -->
        </div>

          <div class="select-menu-no-results">Nothing to show</div>
      </div> <!-- /.select-menu-list -->

      <div class="select-menu-list select-menu-tab-bucket js-select-menu-tab-bucket" data-tab-filter="tags">
        <div data-filterable-for="context-commitish-filter-field" data-filterable-type="substring">


            <div class="select-menu-item js-navigation-item ">
              <span class="select-menu-item-icon octicon octicon-check"></span>
              <a href="/ftlabs/fastclick/tree/v0.6.10/lib/fastclick.js"
                 data-name="v0.6.10"
                 data-skip-pjax="true"
                 rel="nofollow"
                 class="js-navigation-open select-menu-item-text js-select-button-text css-truncate-target"
                 title="v0.6.10">v0.6.10</a>
            </div> <!-- /.select-menu-item -->
            <div class="select-menu-item js-navigation-item ">
              <span class="select-menu-item-icon octicon octicon-check"></span>
              <a href="/ftlabs/fastclick/tree/v0.6.9/lib/fastclick.js"
                 data-name="v0.6.9"
                 data-skip-pjax="true"
                 rel="nofollow"
                 class="js-navigation-open select-menu-item-text js-select-button-text css-truncate-target"
                 title="v0.6.9">v0.6.9</a>
            </div> <!-- /.select-menu-item -->
            <div class="select-menu-item js-navigation-item ">
              <span class="select-menu-item-icon octicon octicon-check"></span>
              <a href="/ftlabs/fastclick/tree/v0.6.8/lib/fastclick.js"
                 data-name="v0.6.8"
                 data-skip-pjax="true"
                 rel="nofollow"
                 class="js-navigation-open select-menu-item-text js-select-button-text css-truncate-target"
                 title="v0.6.8">v0.6.8</a>
            </div> <!-- /.select-menu-item -->
            <div class="select-menu-item js-navigation-item ">
              <span class="select-menu-item-icon octicon octicon-check"></span>
              <a href="/ftlabs/fastclick/tree/v0.6.7/lib/fastclick.js"
                 data-name="v0.6.7"
                 data-skip-pjax="true"
                 rel="nofollow"
                 class="js-navigation-open select-menu-item-text js-select-button-text css-truncate-target"
                 title="v0.6.7">v0.6.7</a>
            </div> <!-- /.select-menu-item -->
            <div class="select-menu-item js-navigation-item ">
              <span class="select-menu-item-icon octicon octicon-check"></span>
              <a href="/ftlabs/fastclick/tree/v0.6.5/lib/fastclick.js"
                 data-name="v0.6.5"
                 data-skip-pjax="true"
                 rel="nofollow"
                 class="js-navigation-open select-menu-item-text js-select-button-text css-truncate-target"
                 title="v0.6.5">v0.6.5</a>
            </div> <!-- /.select-menu-item -->
            <div class="select-menu-item js-navigation-item ">
              <span class="select-menu-item-icon octicon octicon-check"></span>
              <a href="/ftlabs/fastclick/tree/v0.6.4/lib/fastclick.js"
                 data-name="v0.6.4"
                 data-skip-pjax="true"
                 rel="nofollow"
                 class="js-navigation-open select-menu-item-text js-select-button-text css-truncate-target"
                 title="v0.6.4">v0.6.4</a>
            </div> <!-- /.select-menu-item -->
            <div class="select-menu-item js-navigation-item ">
              <span class="select-menu-item-icon octicon octicon-check"></span>
              <a href="/ftlabs/fastclick/tree/v0.6.2/lib/fastclick.js"
                 data-name="v0.6.2"
                 data-skip-pjax="true"
                 rel="nofollow"
                 class="js-navigation-open select-menu-item-text js-select-button-text css-truncate-target"
                 title="v0.6.2">v0.6.2</a>
            </div> <!-- /.select-menu-item -->
            <div class="select-menu-item js-navigation-item ">
              <span class="select-menu-item-icon octicon octicon-check"></span>
              <a href="/ftlabs/fastclick/tree/v0.6.0/lib/fastclick.js"
                 data-name="v0.6.0"
                 data-skip-pjax="true"
                 rel="nofollow"
                 class="js-navigation-open select-menu-item-text js-select-button-text css-truncate-target"
                 title="v0.6.0">v0.6.0</a>
            </div> <!-- /.select-menu-item -->
            <div class="select-menu-item js-navigation-item ">
              <span class="select-menu-item-icon octicon octicon-check"></span>
              <a href="/ftlabs/fastclick/tree/v0.5.6/lib/fastclick.js"
                 data-name="v0.5.6"
                 data-skip-pjax="true"
                 rel="nofollow"
                 class="js-navigation-open select-menu-item-text js-select-button-text css-truncate-target"
                 title="v0.5.6">v0.5.6</a>
            </div> <!-- /.select-menu-item -->
            <div class="select-menu-item js-navigation-item ">
              <span class="select-menu-item-icon octicon octicon-check"></span>
              <a href="/ftlabs/fastclick/tree/v0.5.4/lib/fastclick.js"
                 data-name="v0.5.4"
                 data-skip-pjax="true"
                 rel="nofollow"
                 class="js-navigation-open select-menu-item-text js-select-button-text css-truncate-target"
                 title="v0.5.4">v0.5.4</a>
            </div> <!-- /.select-menu-item -->
            <div class="select-menu-item js-navigation-item ">
              <span class="select-menu-item-icon octicon octicon-check"></span>
              <a href="/ftlabs/fastclick/tree/v0.3.0/lib/fastclick.js"
                 data-name="v0.3.0"
                 data-skip-pjax="true"
                 rel="nofollow"
                 class="js-navigation-open select-menu-item-text js-select-button-text css-truncate-target"
                 title="v0.3.0">v0.3.0</a>
            </div> <!-- /.select-menu-item -->
            <div class="select-menu-item js-navigation-item ">
              <span class="select-menu-item-icon octicon octicon-check"></span>
              <a href="/ftlabs/fastclick/tree/v0.2.0/lib/fastclick.js"
                 data-name="v0.2.0"
                 data-skip-pjax="true"
                 rel="nofollow"
                 class="js-navigation-open select-menu-item-text js-select-button-text css-truncate-target"
                 title="v0.2.0">v0.2.0</a>
            </div> <!-- /.select-menu-item -->
        </div>

        <div class="select-menu-no-results">Nothing to show</div>
      </div> <!-- /.select-menu-list -->

    </div> <!-- /.select-menu-modal -->
  </div> <!-- /.select-menu-modal-holder -->
</div> <!-- /.select-menu -->

  <div class="breadcrumb">
    <span class='repo-root js-repo-root'><span itemscope="" itemtype="http://data-vocabulary.org/Breadcrumb"><a href="/ftlabs/fastclick" data-branch="master" data-direction="back" data-pjax="true" itemscope="url"><span itemprop="title">fastclick</span></a></span></span><span class="separator"> / </span><span itemscope="" itemtype="http://data-vocabulary.org/Breadcrumb"><a href="/ftlabs/fastclick/tree/master/lib" data-branch="master" data-direction="back" data-pjax="true" itemscope="url"><span itemprop="title">lib</span></a></span><span class="separator"> / </span><strong class="final-path">fastclick.js</strong> <span class="js-zeroclipboard minibutton zeroclipboard-button" data-clipboard-text="lib/fastclick.js" data-copied-hint="copied!" title="copy to clipboard"><span class="octicon octicon-clippy"></span></span>
  </div>
</div>



  <div class="commit file-history-tease">
    <img class="main-avatar" height="24" src="https://0.gravatar.com/avatar/34551f266d9bfdeed1e7f287c59f789d?d=https%3A%2F%2Fidenticons.github.com%2F7b6dc47f8d0580ffa3fe9b4061d515cd.png&amp;r=x&amp;s=140" width="24" />
    <span class="author"><a href="/jakearchibald" rel="author">jakearchibald</a></span>
    <time class="js-relative-date" datetime="2013-11-07T01:17:07-08:00" title="2013-11-07 01:17:07">November 07, 2013</time>
    <div class="commit-title">
        <a href="/ftlabs/fastclick/commit/5ff301160d38eb834bd1d0e2240495d273d68183" class="message" data-pjax="true" title="Chrome &gt;= 32 removes 300ms delay on pages that are device-width or less">Chrome &gt;= 32 removes 300ms delay on pages that are device-width or less</a>
    </div>

    <div class="participation">
      <p class="quickstat"><a href="#blob_contributors_box" rel="facebox"><strong>13</strong> contributors</a></p>
          <a class="avatar tooltipped downwards" title="mattcg" href="/ftlabs/fastclick/commits/master/lib/fastclick.js?author=mattcg"><img height="20" src="https://2.gravatar.com/avatar/a618813a55497a791a76363755690df7?d=https%3A%2F%2Fidenticons.github.com%2F70b39c24287eee78696fcac37ed17b73.png&amp;r=x&amp;s=140" width="20" /></a>
    <a class="avatar tooltipped downwards" title="bcamarda" href="/ftlabs/fastclick/commits/master/lib/fastclick.js?author=bcamarda"><img height="20" src="https://0.gravatar.com/avatar/67c5a5250b29cdfc5ca13661204c1c83?d=https%3A%2F%2Fidenticons.github.com%2F730d52a0990951b70b7b0cd4fdae9741.png&amp;r=x&amp;s=140" width="20" /></a>
    <a class="avatar tooltipped downwards" title="wilsonpage" href="/ftlabs/fastclick/commits/master/lib/fastclick.js?author=wilsonpage"><img height="20" src="https://1.gravatar.com/avatar/92ef51ff737e64b627c58bd55fa6e35e?d=https%3A%2F%2Fidenticons.github.com%2F5e644eb198e706b602ec1ff380152fda.png&amp;r=x&amp;s=140" width="20" /></a>
    <a class="avatar tooltipped downwards" title="rowanbeentje" href="/ftlabs/fastclick/commits/master/lib/fastclick.js?author=rowanbeentje"><img height="20" src="https://2.gravatar.com/avatar/6c7c3006da8dad88a29459f7820e154d?d=https%3A%2F%2Fidenticons.github.com%2F173f5713aca00fc12dc2a9d5691b1a39.png&amp;r=x&amp;s=140" width="20" /></a>
    <a class="avatar tooltipped downwards" title="cburgdorf" href="/ftlabs/fastclick/commits/master/lib/fastclick.js?author=cburgdorf"><img height="20" src="https://1.gravatar.com/avatar/1bb69b75c5e73a7c9f4ebbc4835d622f?d=https%3A%2F%2Fidenticons.github.com%2Fc94ab8cc222b741c3b8ca0f9fc60dd3b.png&amp;r=x&amp;s=140" width="20" /></a>
    <a class="avatar tooltipped downwards" title="kpdecker" href="/ftlabs/fastclick/commits/master/lib/fastclick.js?author=kpdecker"><img height="20" src="https://1.gravatar.com/avatar/1d0a77e7fa25279e2ec61a6f802b2862?d=https%3A%2F%2Fidenticons.github.com%2F0cb9827e5625394fd612b96fffc9b357.png&amp;r=x&amp;s=140" width="20" /></a>
    <a class="avatar tooltipped downwards" title="vivekjain10" href="/ftlabs/fastclick/commits/master/lib/fastclick.js?author=vivekjain10"><img height="20" src="https://1.gravatar.com/avatar/95645413b56928d3fe0df0a6c951229d?d=https%3A%2F%2Fidenticons.github.com%2F00456c0abb9a88b7a07d8b28698da6ee.png&amp;r=x&amp;s=140" width="20" /></a>
    <a class="avatar tooltipped downwards" title="maxogden" href="/ftlabs/fastclick/commits/master/lib/fastclick.js?author=maxogden"><img height="20" src="https://2.gravatar.com/avatar/fcc9bbfe2a31c5a6225cc287ed7ae2a6?d=https%3A%2F%2Fidenticons.github.com%2Fd2cec4ae765e49fa93728f323ca5894e.png&amp;r=x&amp;s=140" width="20" /></a>
    <a class="avatar tooltipped downwards" title="jakearchibald" href="/ftlabs/fastclick/commits/master/lib/fastclick.js?author=jakearchibald"><img height="20" src="https://0.gravatar.com/avatar/34551f266d9bfdeed1e7f287c59f789d?d=https%3A%2F%2Fidenticons.github.com%2F7b6dc47f8d0580ffa3fe9b4061d515cd.png&amp;r=x&amp;s=140" width="20" /></a>
    <a class="avatar tooltipped downwards" title="mkai" href="/ftlabs/fastclick/commits/master/lib/fastclick.js?author=mkai"><img height="20" src="https://1.gravatar.com/avatar/bfa143bd7ab6a6a6dcdac9d9edefee52?d=https%3A%2F%2Fidenticons.github.com%2F30281f831f34a09b53711e57b7062b2a.png&amp;r=x&amp;s=140" width="20" /></a>
    <a class="avatar tooltipped downwards" title="mohnish" href="/ftlabs/fastclick/commits/master/lib/fastclick.js?author=mohnish"><img height="20" src="https://1.gravatar.com/avatar/2b17eacef8c3f22ddc6e56389f1b2140?d=https%3A%2F%2Fidenticons.github.com%2F5208bc2efbfd3c982ffbeacf78ebfcb3.png&amp;r=x&amp;s=140" width="20" /></a>
    <a class="avatar tooltipped downwards" title="bensmithett" href="/ftlabs/fastclick/commits/master/lib/fastclick.js?author=bensmithett"><img height="20" src="https://1.gravatar.com/avatar/d8de178c08fe406eceeba711709ed3da?d=https%3A%2F%2Fidenticons.github.com%2Fed56e06a40ce9314cee619699e1f3f2e.png&amp;r=x&amp;s=140" width="20" /></a>
    <a class="avatar tooltipped downwards" title="dr-nafanya" href="/ftlabs/fastclick/commits/master/lib/fastclick.js?author=dr-nafanya"><img height="20" src="https://2.gravatar.com/avatar/b8915d203f550060489a8cc1041b6681?d=https%3A%2F%2Fidenticons.github.com%2F6e1afb79a192c471d12e96c0cfac4c8f.png&amp;r=x&amp;s=140" width="20" /></a>


    </div>
    <div id="blob_contributors_box" style="display:none">
      <h2 class="facebox-header">Users who have contributed to this file</h2>
      <ul class="facebox-user-list">
          <li class="facebox-user-list-item">
            <img height="24" src="https://2.gravatar.com/avatar/a618813a55497a791a76363755690df7?d=https%3A%2F%2Fidenticons.github.com%2F70b39c24287eee78696fcac37ed17b73.png&amp;r=x&amp;s=140" width="24" />
            <a href="/mattcg">mattcg</a>
          </li>
          <li class="facebox-user-list-item">
            <img height="24" src="https://0.gravatar.com/avatar/67c5a5250b29cdfc5ca13661204c1c83?d=https%3A%2F%2Fidenticons.github.com%2F730d52a0990951b70b7b0cd4fdae9741.png&amp;r=x&amp;s=140" width="24" />
            <a href="/bcamarda">bcamarda</a>
          </li>
          <li class="facebox-user-list-item">
            <img height="24" src="https://1.gravatar.com/avatar/92ef51ff737e64b627c58bd55fa6e35e?d=https%3A%2F%2Fidenticons.github.com%2F5e644eb198e706b602ec1ff380152fda.png&amp;r=x&amp;s=140" width="24" />
            <a href="/wilsonpage">wilsonpage</a>
          </li>
          <li class="facebox-user-list-item">
            <img height="24" src="https://2.gravatar.com/avatar/6c7c3006da8dad88a29459f7820e154d?d=https%3A%2F%2Fidenticons.github.com%2F173f5713aca00fc12dc2a9d5691b1a39.png&amp;r=x&amp;s=140" width="24" />
            <a href="/rowanbeentje">rowanbeentje</a>
          </li>
          <li class="facebox-user-list-item">
            <img height="24" src="https://1.gravatar.com/avatar/1bb69b75c5e73a7c9f4ebbc4835d622f?d=https%3A%2F%2Fidenticons.github.com%2Fc94ab8cc222b741c3b8ca0f9fc60dd3b.png&amp;r=x&amp;s=140" width="24" />
            <a href="/cburgdorf">cburgdorf</a>
          </li>
          <li class="facebox-user-list-item">
            <img height="24" src="https://1.gravatar.com/avatar/1d0a77e7fa25279e2ec61a6f802b2862?d=https%3A%2F%2Fidenticons.github.com%2F0cb9827e5625394fd612b96fffc9b357.png&amp;r=x&amp;s=140" width="24" />
            <a href="/kpdecker">kpdecker</a>
          </li>
          <li class="facebox-user-list-item">
            <img height="24" src="https://1.gravatar.com/avatar/95645413b56928d3fe0df0a6c951229d?d=https%3A%2F%2Fidenticons.github.com%2F00456c0abb9a88b7a07d8b28698da6ee.png&amp;r=x&amp;s=140" width="24" />
            <a href="/vivekjain10">vivekjain10</a>
          </li>
          <li class="facebox-user-list-item">
            <img height="24" src="https://2.gravatar.com/avatar/fcc9bbfe2a31c5a6225cc287ed7ae2a6?d=https%3A%2F%2Fidenticons.github.com%2Fd2cec4ae765e49fa93728f323ca5894e.png&amp;r=x&amp;s=140" width="24" />
            <a href="/maxogden">maxogden</a>
          </li>
          <li class="facebox-user-list-item">
            <img height="24" src="https://0.gravatar.com/avatar/34551f266d9bfdeed1e7f287c59f789d?d=https%3A%2F%2Fidenticons.github.com%2F7b6dc47f8d0580ffa3fe9b4061d515cd.png&amp;r=x&amp;s=140" width="24" />
            <a href="/jakearchibald">jakearchibald</a>
          </li>
          <li class="facebox-user-list-item">
            <img height="24" src="https://1.gravatar.com/avatar/bfa143bd7ab6a6a6dcdac9d9edefee52?d=https%3A%2F%2Fidenticons.github.com%2F30281f831f34a09b53711e57b7062b2a.png&amp;r=x&amp;s=140" width="24" />
            <a href="/mkai">mkai</a>
          </li>
          <li class="facebox-user-list-item">
            <img height="24" src="https://1.gravatar.com/avatar/2b17eacef8c3f22ddc6e56389f1b2140?d=https%3A%2F%2Fidenticons.github.com%2F5208bc2efbfd3c982ffbeacf78ebfcb3.png&amp;r=x&amp;s=140" width="24" />
            <a href="/mohnish">mohnish</a>
          </li>
          <li class="facebox-user-list-item">
            <img height="24" src="https://1.gravatar.com/avatar/d8de178c08fe406eceeba711709ed3da?d=https%3A%2F%2Fidenticons.github.com%2Fed56e06a40ce9314cee619699e1f3f2e.png&amp;r=x&amp;s=140" width="24" />
            <a href="/bensmithett">bensmithett</a>
          </li>
          <li class="facebox-user-list-item">
            <img height="24" src="https://2.gravatar.com/avatar/b8915d203f550060489a8cc1041b6681?d=https%3A%2F%2Fidenticons.github.com%2F6e1afb79a192c471d12e96c0cfac4c8f.png&amp;r=x&amp;s=140" width="24" />
            <a href="/dr-nafanya">dr-nafanya</a>
          </li>
      </ul>
    </div>
  </div>

<div id="files" class="bubble">
  <div class="file">
    <div class="meta">
      <div class="info">
        <span class="icon"><b class="octicon octicon-file-text"></b></span>
        <span class="mode" title="File Mode">file</span>
          <span>789 lines (631 sloc)</span>
        <span>23.032 kb</span>
      </div>
      <div class="actions">
        <div class="button-group">
            <a class="minibutton tooltipped leftwards"
               href="github-windows://openRepo/https://github.com/ftlabs/fastclick?branch=master&amp;filepath=lib%2Ffastclick.js" title="Open this file in GitHub for Windows">
                <span class="octicon octicon-device-desktop"></span> Open
            </a>
                <a class="minibutton tooltipped upwards"
                   title="Clicking this button will automatically fork this project so you can edit the file"
                   href="/ftlabs/fastclick/edit/master/lib/fastclick.js"
                   data-method="post" rel="nofollow">Edit</a>
          <a href="/ftlabs/fastclick/raw/master/lib/fastclick.js" class="button minibutton " id="raw-url">Raw</a>
            <a href="/ftlabs/fastclick/blame/master/lib/fastclick.js" class="button minibutton ">Blame</a>
          <a href="/ftlabs/fastclick/commits/master/lib/fastclick.js" class="button minibutton " rel="nofollow">History</a>
        </div><!-- /.button-group -->
          <a class="minibutton danger empty-icon tooltipped downwards"
             href="/ftlabs/fastclick/delete/master/lib/fastclick.js"
             title="Fork this project and delete file"
             data-method="post" data-test-id="delete-blob-file" rel="nofollow">
          Delete
        </a>
      </div><!-- /.actions -->

    </div>
        <div class="blob-wrapper data type-javascript js-blob-data">
        <table class="file-code file-diff">
          <tr class="file-code-line">
            <td class="blob-line-nums">
              <span id="L1" rel="#L1">1</span>
<span id="L2" rel="#L2">2</span>
<span id="L3" rel="#L3">3</span>
<span id="L4" rel="#L4">4</span>
<span id="L5" rel="#L5">5</span>
<span id="L6" rel="#L6">6</span>
<span id="L7" rel="#L7">7</span>
<span id="L8" rel="#L8">8</span>
<span id="L9" rel="#L9">9</span>
<span id="L10" rel="#L10">10</span>
<span id="L11" rel="#L11">11</span>
<span id="L12" rel="#L12">12</span>
<span id="L13" rel="#L13">13</span>
<span id="L14" rel="#L14">14</span>
<span id="L15" rel="#L15">15</span>
<span id="L16" rel="#L16">16</span>
<span id="L17" rel="#L17">17</span>
<span id="L18" rel="#L18">18</span>
<span id="L19" rel="#L19">19</span>
<span id="L20" rel="#L20">20</span>
<span id="L21" rel="#L21">21</span>
<span id="L22" rel="#L22">22</span>
<span id="L23" rel="#L23">23</span>
<span id="L24" rel="#L24">24</span>
<span id="L25" rel="#L25">25</span>
<span id="L26" rel="#L26">26</span>
<span id="L27" rel="#L27">27</span>
<span id="L28" rel="#L28">28</span>
<span id="L29" rel="#L29">29</span>
<span id="L30" rel="#L30">30</span>
<span id="L31" rel="#L31">31</span>
<span id="L32" rel="#L32">32</span>
<span id="L33" rel="#L33">33</span>
<span id="L34" rel="#L34">34</span>
<span id="L35" rel="#L35">35</span>
<span id="L36" rel="#L36">36</span>
<span id="L37" rel="#L37">37</span>
<span id="L38" rel="#L38">38</span>
<span id="L39" rel="#L39">39</span>
<span id="L40" rel="#L40">40</span>
<span id="L41" rel="#L41">41</span>
<span id="L42" rel="#L42">42</span>
<span id="L43" rel="#L43">43</span>
<span id="L44" rel="#L44">44</span>
<span id="L45" rel="#L45">45</span>
<span id="L46" rel="#L46">46</span>
<span id="L47" rel="#L47">47</span>
<span id="L48" rel="#L48">48</span>
<span id="L49" rel="#L49">49</span>
<span id="L50" rel="#L50">50</span>
<span id="L51" rel="#L51">51</span>
<span id="L52" rel="#L52">52</span>
<span id="L53" rel="#L53">53</span>
<span id="L54" rel="#L54">54</span>
<span id="L55" rel="#L55">55</span>
<span id="L56" rel="#L56">56</span>
<span id="L57" rel="#L57">57</span>
<span id="L58" rel="#L58">58</span>
<span id="L59" rel="#L59">59</span>
<span id="L60" rel="#L60">60</span>
<span id="L61" rel="#L61">61</span>
<span id="L62" rel="#L62">62</span>
<span id="L63" rel="#L63">63</span>
<span id="L64" rel="#L64">64</span>
<span id="L65" rel="#L65">65</span>
<span id="L66" rel="#L66">66</span>
<span id="L67" rel="#L67">67</span>
<span id="L68" rel="#L68">68</span>
<span id="L69" rel="#L69">69</span>
<span id="L70" rel="#L70">70</span>
<span id="L71" rel="#L71">71</span>
<span id="L72" rel="#L72">72</span>
<span id="L73" rel="#L73">73</span>
<span id="L74" rel="#L74">74</span>
<span id="L75" rel="#L75">75</span>
<span id="L76" rel="#L76">76</span>
<span id="L77" rel="#L77">77</span>
<span id="L78" rel="#L78">78</span>
<span id="L79" rel="#L79">79</span>
<span id="L80" rel="#L80">80</span>
<span id="L81" rel="#L81">81</span>
<span id="L82" rel="#L82">82</span>
<span id="L83" rel="#L83">83</span>
<span id="L84" rel="#L84">84</span>
<span id="L85" rel="#L85">85</span>
<span id="L86" rel="#L86">86</span>
<span id="L87" rel="#L87">87</span>
<span id="L88" rel="#L88">88</span>
<span id="L89" rel="#L89">89</span>
<span id="L90" rel="#L90">90</span>
<span id="L91" rel="#L91">91</span>
<span id="L92" rel="#L92">92</span>
<span id="L93" rel="#L93">93</span>
<span id="L94" rel="#L94">94</span>
<span id="L95" rel="#L95">95</span>
<span id="L96" rel="#L96">96</span>
<span id="L97" rel="#L97">97</span>
<span id="L98" rel="#L98">98</span>
<span id="L99" rel="#L99">99</span>
<span id="L100" rel="#L100">100</span>
<span id="L101" rel="#L101">101</span>
<span id="L102" rel="#L102">102</span>
<span id="L103" rel="#L103">103</span>
<span id="L104" rel="#L104">104</span>
<span id="L105" rel="#L105">105</span>
<span id="L106" rel="#L106">106</span>
<span id="L107" rel="#L107">107</span>
<span id="L108" rel="#L108">108</span>
<span id="L109" rel="#L109">109</span>
<span id="L110" rel="#L110">110</span>
<span id="L111" rel="#L111">111</span>
<span id="L112" rel="#L112">112</span>
<span id="L113" rel="#L113">113</span>
<span id="L114" rel="#L114">114</span>
<span id="L115" rel="#L115">115</span>
<span id="L116" rel="#L116">116</span>
<span id="L117" rel="#L117">117</span>
<span id="L118" rel="#L118">118</span>
<span id="L119" rel="#L119">119</span>
<span id="L120" rel="#L120">120</span>
<span id="L121" rel="#L121">121</span>
<span id="L122" rel="#L122">122</span>
<span id="L123" rel="#L123">123</span>
<span id="L124" rel="#L124">124</span>
<span id="L125" rel="#L125">125</span>
<span id="L126" rel="#L126">126</span>
<span id="L127" rel="#L127">127</span>
<span id="L128" rel="#L128">128</span>
<span id="L129" rel="#L129">129</span>
<span id="L130" rel="#L130">130</span>
<span id="L131" rel="#L131">131</span>
<span id="L132" rel="#L132">132</span>
<span id="L133" rel="#L133">133</span>
<span id="L134" rel="#L134">134</span>
<span id="L135" rel="#L135">135</span>
<span id="L136" rel="#L136">136</span>
<span id="L137" rel="#L137">137</span>
<span id="L138" rel="#L138">138</span>
<span id="L139" rel="#L139">139</span>
<span id="L140" rel="#L140">140</span>
<span id="L141" rel="#L141">141</span>
<span id="L142" rel="#L142">142</span>
<span id="L143" rel="#L143">143</span>
<span id="L144" rel="#L144">144</span>
<span id="L145" rel="#L145">145</span>
<span id="L146" rel="#L146">146</span>
<span id="L147" rel="#L147">147</span>
<span id="L148" rel="#L148">148</span>
<span id="L149" rel="#L149">149</span>
<span id="L150" rel="#L150">150</span>
<span id="L151" rel="#L151">151</span>
<span id="L152" rel="#L152">152</span>
<span id="L153" rel="#L153">153</span>
<span id="L154" rel="#L154">154</span>
<span id="L155" rel="#L155">155</span>
<span id="L156" rel="#L156">156</span>
<span id="L157" rel="#L157">157</span>
<span id="L158" rel="#L158">158</span>
<span id="L159" rel="#L159">159</span>
<span id="L160" rel="#L160">160</span>
<span id="L161" rel="#L161">161</span>
<span id="L162" rel="#L162">162</span>
<span id="L163" rel="#L163">163</span>
<span id="L164" rel="#L164">164</span>
<span id="L165" rel="#L165">165</span>
<span id="L166" rel="#L166">166</span>
<span id="L167" rel="#L167">167</span>
<span id="L168" rel="#L168">168</span>
<span id="L169" rel="#L169">169</span>
<span id="L170" rel="#L170">170</span>
<span id="L171" rel="#L171">171</span>
<span id="L172" rel="#L172">172</span>
<span id="L173" rel="#L173">173</span>
<span id="L174" rel="#L174">174</span>
<span id="L175" rel="#L175">175</span>
<span id="L176" rel="#L176">176</span>
<span id="L177" rel="#L177">177</span>
<span id="L178" rel="#L178">178</span>
<span id="L179" rel="#L179">179</span>
<span id="L180" rel="#L180">180</span>
<span id="L181" rel="#L181">181</span>
<span id="L182" rel="#L182">182</span>
<span id="L183" rel="#L183">183</span>
<span id="L184" rel="#L184">184</span>
<span id="L185" rel="#L185">185</span>
<span id="L186" rel="#L186">186</span>
<span id="L187" rel="#L187">187</span>
<span id="L188" rel="#L188">188</span>
<span id="L189" rel="#L189">189</span>
<span id="L190" rel="#L190">190</span>
<span id="L191" rel="#L191">191</span>
<span id="L192" rel="#L192">192</span>
<span id="L193" rel="#L193">193</span>
<span id="L194" rel="#L194">194</span>
<span id="L195" rel="#L195">195</span>
<span id="L196" rel="#L196">196</span>
<span id="L197" rel="#L197">197</span>
<span id="L198" rel="#L198">198</span>
<span id="L199" rel="#L199">199</span>
<span id="L200" rel="#L200">200</span>
<span id="L201" rel="#L201">201</span>
<span id="L202" rel="#L202">202</span>
<span id="L203" rel="#L203">203</span>
<span id="L204" rel="#L204">204</span>
<span id="L205" rel="#L205">205</span>
<span id="L206" rel="#L206">206</span>
<span id="L207" rel="#L207">207</span>
<span id="L208" rel="#L208">208</span>
<span id="L209" rel="#L209">209</span>
<span id="L210" rel="#L210">210</span>
<span id="L211" rel="#L211">211</span>
<span id="L212" rel="#L212">212</span>
<span id="L213" rel="#L213">213</span>
<span id="L214" rel="#L214">214</span>
<span id="L215" rel="#L215">215</span>
<span id="L216" rel="#L216">216</span>
<span id="L217" rel="#L217">217</span>
<span id="L218" rel="#L218">218</span>
<span id="L219" rel="#L219">219</span>
<span id="L220" rel="#L220">220</span>
<span id="L221" rel="#L221">221</span>
<span id="L222" rel="#L222">222</span>
<span id="L223" rel="#L223">223</span>
<span id="L224" rel="#L224">224</span>
<span id="L225" rel="#L225">225</span>
<span id="L226" rel="#L226">226</span>
<span id="L227" rel="#L227">227</span>
<span id="L228" rel="#L228">228</span>
<span id="L229" rel="#L229">229</span>
<span id="L230" rel="#L230">230</span>
<span id="L231" rel="#L231">231</span>
<span id="L232" rel="#L232">232</span>
<span id="L233" rel="#L233">233</span>
<span id="L234" rel="#L234">234</span>
<span id="L235" rel="#L235">235</span>
<span id="L236" rel="#L236">236</span>
<span id="L237" rel="#L237">237</span>
<span id="L238" rel="#L238">238</span>
<span id="L239" rel="#L239">239</span>
<span id="L240" rel="#L240">240</span>
<span id="L241" rel="#L241">241</span>
<span id="L242" rel="#L242">242</span>
<span id="L243" rel="#L243">243</span>
<span id="L244" rel="#L244">244</span>
<span id="L245" rel="#L245">245</span>
<span id="L246" rel="#L246">246</span>
<span id="L247" rel="#L247">247</span>
<span id="L248" rel="#L248">248</span>
<span id="L249" rel="#L249">249</span>
<span id="L250" rel="#L250">250</span>
<span id="L251" rel="#L251">251</span>
<span id="L252" rel="#L252">252</span>
<span id="L253" rel="#L253">253</span>
<span id="L254" rel="#L254">254</span>
<span id="L255" rel="#L255">255</span>
<span id="L256" rel="#L256">256</span>
<span id="L257" rel="#L257">257</span>
<span id="L258" rel="#L258">258</span>
<span id="L259" rel="#L259">259</span>
<span id="L260" rel="#L260">260</span>
<span id="L261" rel="#L261">261</span>
<span id="L262" rel="#L262">262</span>
<span id="L263" rel="#L263">263</span>
<span id="L264" rel="#L264">264</span>
<span id="L265" rel="#L265">265</span>
<span id="L266" rel="#L266">266</span>
<span id="L267" rel="#L267">267</span>
<span id="L268" rel="#L268">268</span>
<span id="L269" rel="#L269">269</span>
<span id="L270" rel="#L270">270</span>
<span id="L271" rel="#L271">271</span>
<span id="L272" rel="#L272">272</span>
<span id="L273" rel="#L273">273</span>
<span id="L274" rel="#L274">274</span>
<span id="L275" rel="#L275">275</span>
<span id="L276" rel="#L276">276</span>
<span id="L277" rel="#L277">277</span>
<span id="L278" rel="#L278">278</span>
<span id="L279" rel="#L279">279</span>
<span id="L280" rel="#L280">280</span>
<span id="L281" rel="#L281">281</span>
<span id="L282" rel="#L282">282</span>
<span id="L283" rel="#L283">283</span>
<span id="L284" rel="#L284">284</span>
<span id="L285" rel="#L285">285</span>
<span id="L286" rel="#L286">286</span>
<span id="L287" rel="#L287">287</span>
<span id="L288" rel="#L288">288</span>
<span id="L289" rel="#L289">289</span>
<span id="L290" rel="#L290">290</span>
<span id="L291" rel="#L291">291</span>
<span id="L292" rel="#L292">292</span>
<span id="L293" rel="#L293">293</span>
<span id="L294" rel="#L294">294</span>
<span id="L295" rel="#L295">295</span>
<span id="L296" rel="#L296">296</span>
<span id="L297" rel="#L297">297</span>
<span id="L298" rel="#L298">298</span>
<span id="L299" rel="#L299">299</span>
<span id="L300" rel="#L300">300</span>
<span id="L301" rel="#L301">301</span>
<span id="L302" rel="#L302">302</span>
<span id="L303" rel="#L303">303</span>
<span id="L304" rel="#L304">304</span>
<span id="L305" rel="#L305">305</span>
<span id="L306" rel="#L306">306</span>
<span id="L307" rel="#L307">307</span>
<span id="L308" rel="#L308">308</span>
<span id="L309" rel="#L309">309</span>
<span id="L310" rel="#L310">310</span>
<span id="L311" rel="#L311">311</span>
<span id="L312" rel="#L312">312</span>
<span id="L313" rel="#L313">313</span>
<span id="L314" rel="#L314">314</span>
<span id="L315" rel="#L315">315</span>
<span id="L316" rel="#L316">316</span>
<span id="L317" rel="#L317">317</span>
<span id="L318" rel="#L318">318</span>
<span id="L319" rel="#L319">319</span>
<span id="L320" rel="#L320">320</span>
<span id="L321" rel="#L321">321</span>
<span id="L322" rel="#L322">322</span>
<span id="L323" rel="#L323">323</span>
<span id="L324" rel="#L324">324</span>
<span id="L325" rel="#L325">325</span>
<span id="L326" rel="#L326">326</span>
<span id="L327" rel="#L327">327</span>
<span id="L328" rel="#L328">328</span>
<span id="L329" rel="#L329">329</span>
<span id="L330" rel="#L330">330</span>
<span id="L331" rel="#L331">331</span>
<span id="L332" rel="#L332">332</span>
<span id="L333" rel="#L333">333</span>
<span id="L334" rel="#L334">334</span>
<span id="L335" rel="#L335">335</span>
<span id="L336" rel="#L336">336</span>
<span id="L337" rel="#L337">337</span>
<span id="L338" rel="#L338">338</span>
<span id="L339" rel="#L339">339</span>
<span id="L340" rel="#L340">340</span>
<span id="L341" rel="#L341">341</span>
<span id="L342" rel="#L342">342</span>
<span id="L343" rel="#L343">343</span>
<span id="L344" rel="#L344">344</span>
<span id="L345" rel="#L345">345</span>
<span id="L346" rel="#L346">346</span>
<span id="L347" rel="#L347">347</span>
<span id="L348" rel="#L348">348</span>
<span id="L349" rel="#L349">349</span>
<span id="L350" rel="#L350">350</span>
<span id="L351" rel="#L351">351</span>
<span id="L352" rel="#L352">352</span>
<span id="L353" rel="#L353">353</span>
<span id="L354" rel="#L354">354</span>
<span id="L355" rel="#L355">355</span>
<span id="L356" rel="#L356">356</span>
<span id="L357" rel="#L357">357</span>
<span id="L358" rel="#L358">358</span>
<span id="L359" rel="#L359">359</span>
<span id="L360" rel="#L360">360</span>
<span id="L361" rel="#L361">361</span>
<span id="L362" rel="#L362">362</span>
<span id="L363" rel="#L363">363</span>
<span id="L364" rel="#L364">364</span>
<span id="L365" rel="#L365">365</span>
<span id="L366" rel="#L366">366</span>
<span id="L367" rel="#L367">367</span>
<span id="L368" rel="#L368">368</span>
<span id="L369" rel="#L369">369</span>
<span id="L370" rel="#L370">370</span>
<span id="L371" rel="#L371">371</span>
<span id="L372" rel="#L372">372</span>
<span id="L373" rel="#L373">373</span>
<span id="L374" rel="#L374">374</span>
<span id="L375" rel="#L375">375</span>
<span id="L376" rel="#L376">376</span>
<span id="L377" rel="#L377">377</span>
<span id="L378" rel="#L378">378</span>
<span id="L379" rel="#L379">379</span>
<span id="L380" rel="#L380">380</span>
<span id="L381" rel="#L381">381</span>
<span id="L382" rel="#L382">382</span>
<span id="L383" rel="#L383">383</span>
<span id="L384" rel="#L384">384</span>
<span id="L385" rel="#L385">385</span>
<span id="L386" rel="#L386">386</span>
<span id="L387" rel="#L387">387</span>
<span id="L388" rel="#L388">388</span>
<span id="L389" rel="#L389">389</span>
<span id="L390" rel="#L390">390</span>
<span id="L391" rel="#L391">391</span>
<span id="L392" rel="#L392">392</span>
<span id="L393" rel="#L393">393</span>
<span id="L394" rel="#L394">394</span>
<span id="L395" rel="#L395">395</span>
<span id="L396" rel="#L396">396</span>
<span id="L397" rel="#L397">397</span>
<span id="L398" rel="#L398">398</span>
<span id="L399" rel="#L399">399</span>
<span id="L400" rel="#L400">400</span>
<span id="L401" rel="#L401">401</span>
<span id="L402" rel="#L402">402</span>
<span id="L403" rel="#L403">403</span>
<span id="L404" rel="#L404">404</span>
<span id="L405" rel="#L405">405</span>
<span id="L406" rel="#L406">406</span>
<span id="L407" rel="#L407">407</span>
<span id="L408" rel="#L408">408</span>
<span id="L409" rel="#L409">409</span>
<span id="L410" rel="#L410">410</span>
<span id="L411" rel="#L411">411</span>
<span id="L412" rel="#L412">412</span>
<span id="L413" rel="#L413">413</span>
<span id="L414" rel="#L414">414</span>
<span id="L415" rel="#L415">415</span>
<span id="L416" rel="#L416">416</span>
<span id="L417" rel="#L417">417</span>
<span id="L418" rel="#L418">418</span>
<span id="L419" rel="#L419">419</span>
<span id="L420" rel="#L420">420</span>
<span id="L421" rel="#L421">421</span>
<span id="L422" rel="#L422">422</span>
<span id="L423" rel="#L423">423</span>
<span id="L424" rel="#L424">424</span>
<span id="L425" rel="#L425">425</span>
<span id="L426" rel="#L426">426</span>
<span id="L427" rel="#L427">427</span>
<span id="L428" rel="#L428">428</span>
<span id="L429" rel="#L429">429</span>
<span id="L430" rel="#L430">430</span>
<span id="L431" rel="#L431">431</span>
<span id="L432" rel="#L432">432</span>
<span id="L433" rel="#L433">433</span>
<span id="L434" rel="#L434">434</span>
<span id="L435" rel="#L435">435</span>
<span id="L436" rel="#L436">436</span>
<span id="L437" rel="#L437">437</span>
<span id="L438" rel="#L438">438</span>
<span id="L439" rel="#L439">439</span>
<span id="L440" rel="#L440">440</span>
<span id="L441" rel="#L441">441</span>
<span id="L442" rel="#L442">442</span>
<span id="L443" rel="#L443">443</span>
<span id="L444" rel="#L444">444</span>
<span id="L445" rel="#L445">445</span>
<span id="L446" rel="#L446">446</span>
<span id="L447" rel="#L447">447</span>
<span id="L448" rel="#L448">448</span>
<span id="L449" rel="#L449">449</span>
<span id="L450" rel="#L450">450</span>
<span id="L451" rel="#L451">451</span>
<span id="L452" rel="#L452">452</span>
<span id="L453" rel="#L453">453</span>
<span id="L454" rel="#L454">454</span>
<span id="L455" rel="#L455">455</span>
<span id="L456" rel="#L456">456</span>
<span id="L457" rel="#L457">457</span>
<span id="L458" rel="#L458">458</span>
<span id="L459" rel="#L459">459</span>
<span id="L460" rel="#L460">460</span>
<span id="L461" rel="#L461">461</span>
<span id="L462" rel="#L462">462</span>
<span id="L463" rel="#L463">463</span>
<span id="L464" rel="#L464">464</span>
<span id="L465" rel="#L465">465</span>
<span id="L466" rel="#L466">466</span>
<span id="L467" rel="#L467">467</span>
<span id="L468" rel="#L468">468</span>
<span id="L469" rel="#L469">469</span>
<span id="L470" rel="#L470">470</span>
<span id="L471" rel="#L471">471</span>
<span id="L472" rel="#L472">472</span>
<span id="L473" rel="#L473">473</span>
<span id="L474" rel="#L474">474</span>
<span id="L475" rel="#L475">475</span>
<span id="L476" rel="#L476">476</span>
<span id="L477" rel="#L477">477</span>
<span id="L478" rel="#L478">478</span>
<span id="L479" rel="#L479">479</span>
<span id="L480" rel="#L480">480</span>
<span id="L481" rel="#L481">481</span>
<span id="L482" rel="#L482">482</span>
<span id="L483" rel="#L483">483</span>
<span id="L484" rel="#L484">484</span>
<span id="L485" rel="#L485">485</span>
<span id="L486" rel="#L486">486</span>
<span id="L487" rel="#L487">487</span>
<span id="L488" rel="#L488">488</span>
<span id="L489" rel="#L489">489</span>
<span id="L490" rel="#L490">490</span>
<span id="L491" rel="#L491">491</span>
<span id="L492" rel="#L492">492</span>
<span id="L493" rel="#L493">493</span>
<span id="L494" rel="#L494">494</span>
<span id="L495" rel="#L495">495</span>
<span id="L496" rel="#L496">496</span>
<span id="L497" rel="#L497">497</span>
<span id="L498" rel="#L498">498</span>
<span id="L499" rel="#L499">499</span>
<span id="L500" rel="#L500">500</span>
<span id="L501" rel="#L501">501</span>
<span id="L502" rel="#L502">502</span>
<span id="L503" rel="#L503">503</span>
<span id="L504" rel="#L504">504</span>
<span id="L505" rel="#L505">505</span>
<span id="L506" rel="#L506">506</span>
<span id="L507" rel="#L507">507</span>
<span id="L508" rel="#L508">508</span>
<span id="L509" rel="#L509">509</span>
<span id="L510" rel="#L510">510</span>
<span id="L511" rel="#L511">511</span>
<span id="L512" rel="#L512">512</span>
<span id="L513" rel="#L513">513</span>
<span id="L514" rel="#L514">514</span>
<span id="L515" rel="#L515">515</span>
<span id="L516" rel="#L516">516</span>
<span id="L517" rel="#L517">517</span>
<span id="L518" rel="#L518">518</span>
<span id="L519" rel="#L519">519</span>
<span id="L520" rel="#L520">520</span>
<span id="L521" rel="#L521">521</span>
<span id="L522" rel="#L522">522</span>
<span id="L523" rel="#L523">523</span>
<span id="L524" rel="#L524">524</span>
<span id="L525" rel="#L525">525</span>
<span id="L526" rel="#L526">526</span>
<span id="L527" rel="#L527">527</span>
<span id="L528" rel="#L528">528</span>
<span id="L529" rel="#L529">529</span>
<span id="L530" rel="#L530">530</span>
<span id="L531" rel="#L531">531</span>
<span id="L532" rel="#L532">532</span>
<span id="L533" rel="#L533">533</span>
<span id="L534" rel="#L534">534</span>
<span id="L535" rel="#L535">535</span>
<span id="L536" rel="#L536">536</span>
<span id="L537" rel="#L537">537</span>
<span id="L538" rel="#L538">538</span>
<span id="L539" rel="#L539">539</span>
<span id="L540" rel="#L540">540</span>
<span id="L541" rel="#L541">541</span>
<span id="L542" rel="#L542">542</span>
<span id="L543" rel="#L543">543</span>
<span id="L544" rel="#L544">544</span>
<span id="L545" rel="#L545">545</span>
<span id="L546" rel="#L546">546</span>
<span id="L547" rel="#L547">547</span>
<span id="L548" rel="#L548">548</span>
<span id="L549" rel="#L549">549</span>
<span id="L550" rel="#L550">550</span>
<span id="L551" rel="#L551">551</span>
<span id="L552" rel="#L552">552</span>
<span id="L553" rel="#L553">553</span>
<span id="L554" rel="#L554">554</span>
<span id="L555" rel="#L555">555</span>
<span id="L556" rel="#L556">556</span>
<span id="L557" rel="#L557">557</span>
<span id="L558" rel="#L558">558</span>
<span id="L559" rel="#L559">559</span>
<span id="L560" rel="#L560">560</span>
<span id="L561" rel="#L561">561</span>
<span id="L562" rel="#L562">562</span>
<span id="L563" rel="#L563">563</span>
<span id="L564" rel="#L564">564</span>
<span id="L565" rel="#L565">565</span>
<span id="L566" rel="#L566">566</span>
<span id="L567" rel="#L567">567</span>
<span id="L568" rel="#L568">568</span>
<span id="L569" rel="#L569">569</span>
<span id="L570" rel="#L570">570</span>
<span id="L571" rel="#L571">571</span>
<span id="L572" rel="#L572">572</span>
<span id="L573" rel="#L573">573</span>
<span id="L574" rel="#L574">574</span>
<span id="L575" rel="#L575">575</span>
<span id="L576" rel="#L576">576</span>
<span id="L577" rel="#L577">577</span>
<span id="L578" rel="#L578">578</span>
<span id="L579" rel="#L579">579</span>
<span id="L580" rel="#L580">580</span>
<span id="L581" rel="#L581">581</span>
<span id="L582" rel="#L582">582</span>
<span id="L583" rel="#L583">583</span>
<span id="L584" rel="#L584">584</span>
<span id="L585" rel="#L585">585</span>
<span id="L586" rel="#L586">586</span>
<span id="L587" rel="#L587">587</span>
<span id="L588" rel="#L588">588</span>
<span id="L589" rel="#L589">589</span>
<span id="L590" rel="#L590">590</span>
<span id="L591" rel="#L591">591</span>
<span id="L592" rel="#L592">592</span>
<span id="L593" rel="#L593">593</span>
<span id="L594" rel="#L594">594</span>
<span id="L595" rel="#L595">595</span>
<span id="L596" rel="#L596">596</span>
<span id="L597" rel="#L597">597</span>
<span id="L598" rel="#L598">598</span>
<span id="L599" rel="#L599">599</span>
<span id="L600" rel="#L600">600</span>
<span id="L601" rel="#L601">601</span>
<span id="L602" rel="#L602">602</span>
<span id="L603" rel="#L603">603</span>
<span id="L604" rel="#L604">604</span>
<span id="L605" rel="#L605">605</span>
<span id="L606" rel="#L606">606</span>
<span id="L607" rel="#L607">607</span>
<span id="L608" rel="#L608">608</span>
<span id="L609" rel="#L609">609</span>
<span id="L610" rel="#L610">610</span>
<span id="L611" rel="#L611">611</span>
<span id="L612" rel="#L612">612</span>
<span id="L613" rel="#L613">613</span>
<span id="L614" rel="#L614">614</span>
<span id="L615" rel="#L615">615</span>
<span id="L616" rel="#L616">616</span>
<span id="L617" rel="#L617">617</span>
<span id="L618" rel="#L618">618</span>
<span id="L619" rel="#L619">619</span>
<span id="L620" rel="#L620">620</span>
<span id="L621" rel="#L621">621</span>
<span id="L622" rel="#L622">622</span>
<span id="L623" rel="#L623">623</span>
<span id="L624" rel="#L624">624</span>
<span id="L625" rel="#L625">625</span>
<span id="L626" rel="#L626">626</span>
<span id="L627" rel="#L627">627</span>
<span id="L628" rel="#L628">628</span>
<span id="L629" rel="#L629">629</span>
<span id="L630" rel="#L630">630</span>
<span id="L631" rel="#L631">631</span>
<span id="L632" rel="#L632">632</span>
<span id="L633" rel="#L633">633</span>
<span id="L634" rel="#L634">634</span>
<span id="L635" rel="#L635">635</span>
<span id="L636" rel="#L636">636</span>
<span id="L637" rel="#L637">637</span>
<span id="L638" rel="#L638">638</span>
<span id="L639" rel="#L639">639</span>
<span id="L640" rel="#L640">640</span>
<span id="L641" rel="#L641">641</span>
<span id="L642" rel="#L642">642</span>
<span id="L643" rel="#L643">643</span>
<span id="L644" rel="#L644">644</span>
<span id="L645" rel="#L645">645</span>
<span id="L646" rel="#L646">646</span>
<span id="L647" rel="#L647">647</span>
<span id="L648" rel="#L648">648</span>
<span id="L649" rel="#L649">649</span>
<span id="L650" rel="#L650">650</span>
<span id="L651" rel="#L651">651</span>
<span id="L652" rel="#L652">652</span>
<span id="L653" rel="#L653">653</span>
<span id="L654" rel="#L654">654</span>
<span id="L655" rel="#L655">655</span>
<span id="L656" rel="#L656">656</span>
<span id="L657" rel="#L657">657</span>
<span id="L658" rel="#L658">658</span>
<span id="L659" rel="#L659">659</span>
<span id="L660" rel="#L660">660</span>
<span id="L661" rel="#L661">661</span>
<span id="L662" rel="#L662">662</span>
<span id="L663" rel="#L663">663</span>
<span id="L664" rel="#L664">664</span>
<span id="L665" rel="#L665">665</span>
<span id="L666" rel="#L666">666</span>
<span id="L667" rel="#L667">667</span>
<span id="L668" rel="#L668">668</span>
<span id="L669" rel="#L669">669</span>
<span id="L670" rel="#L670">670</span>
<span id="L671" rel="#L671">671</span>
<span id="L672" rel="#L672">672</span>
<span id="L673" rel="#L673">673</span>
<span id="L674" rel="#L674">674</span>
<span id="L675" rel="#L675">675</span>
<span id="L676" rel="#L676">676</span>
<span id="L677" rel="#L677">677</span>
<span id="L678" rel="#L678">678</span>
<span id="L679" rel="#L679">679</span>
<span id="L680" rel="#L680">680</span>
<span id="L681" rel="#L681">681</span>
<span id="L682" rel="#L682">682</span>
<span id="L683" rel="#L683">683</span>
<span id="L684" rel="#L684">684</span>
<span id="L685" rel="#L685">685</span>
<span id="L686" rel="#L686">686</span>
<span id="L687" rel="#L687">687</span>
<span id="L688" rel="#L688">688</span>
<span id="L689" rel="#L689">689</span>
<span id="L690" rel="#L690">690</span>
<span id="L691" rel="#L691">691</span>
<span id="L692" rel="#L692">692</span>
<span id="L693" rel="#L693">693</span>
<span id="L694" rel="#L694">694</span>
<span id="L695" rel="#L695">695</span>
<span id="L696" rel="#L696">696</span>
<span id="L697" rel="#L697">697</span>
<span id="L698" rel="#L698">698</span>
<span id="L699" rel="#L699">699</span>
<span id="L700" rel="#L700">700</span>
<span id="L701" rel="#L701">701</span>
<span id="L702" rel="#L702">702</span>
<span id="L703" rel="#L703">703</span>
<span id="L704" rel="#L704">704</span>
<span id="L705" rel="#L705">705</span>
<span id="L706" rel="#L706">706</span>
<span id="L707" rel="#L707">707</span>
<span id="L708" rel="#L708">708</span>
<span id="L709" rel="#L709">709</span>
<span id="L710" rel="#L710">710</span>
<span id="L711" rel="#L711">711</span>
<span id="L712" rel="#L712">712</span>
<span id="L713" rel="#L713">713</span>
<span id="L714" rel="#L714">714</span>
<span id="L715" rel="#L715">715</span>
<span id="L716" rel="#L716">716</span>
<span id="L717" rel="#L717">717</span>
<span id="L718" rel="#L718">718</span>
<span id="L719" rel="#L719">719</span>
<span id="L720" rel="#L720">720</span>
<span id="L721" rel="#L721">721</span>
<span id="L722" rel="#L722">722</span>
<span id="L723" rel="#L723">723</span>
<span id="L724" rel="#L724">724</span>
<span id="L725" rel="#L725">725</span>
<span id="L726" rel="#L726">726</span>
<span id="L727" rel="#L727">727</span>
<span id="L728" rel="#L728">728</span>
<span id="L729" rel="#L729">729</span>
<span id="L730" rel="#L730">730</span>
<span id="L731" rel="#L731">731</span>
<span id="L732" rel="#L732">732</span>
<span id="L733" rel="#L733">733</span>
<span id="L734" rel="#L734">734</span>
<span id="L735" rel="#L735">735</span>
<span id="L736" rel="#L736">736</span>
<span id="L737" rel="#L737">737</span>
<span id="L738" rel="#L738">738</span>
<span id="L739" rel="#L739">739</span>
<span id="L740" rel="#L740">740</span>
<span id="L741" rel="#L741">741</span>
<span id="L742" rel="#L742">742</span>
<span id="L743" rel="#L743">743</span>
<span id="L744" rel="#L744">744</span>
<span id="L745" rel="#L745">745</span>
<span id="L746" rel="#L746">746</span>
<span id="L747" rel="#L747">747</span>
<span id="L748" rel="#L748">748</span>
<span id="L749" rel="#L749">749</span>
<span id="L750" rel="#L750">750</span>
<span id="L751" rel="#L751">751</span>
<span id="L752" rel="#L752">752</span>
<span id="L753" rel="#L753">753</span>
<span id="L754" rel="#L754">754</span>
<span id="L755" rel="#L755">755</span>
<span id="L756" rel="#L756">756</span>
<span id="L757" rel="#L757">757</span>
<span id="L758" rel="#L758">758</span>
<span id="L759" rel="#L759">759</span>
<span id="L760" rel="#L760">760</span>
<span id="L761" rel="#L761">761</span>
<span id="L762" rel="#L762">762</span>
<span id="L763" rel="#L763">763</span>
<span id="L764" rel="#L764">764</span>
<span id="L765" rel="#L765">765</span>
<span id="L766" rel="#L766">766</span>
<span id="L767" rel="#L767">767</span>
<span id="L768" rel="#L768">768</span>
<span id="L769" rel="#L769">769</span>
<span id="L770" rel="#L770">770</span>
<span id="L771" rel="#L771">771</span>
<span id="L772" rel="#L772">772</span>
<span id="L773" rel="#L773">773</span>
<span id="L774" rel="#L774">774</span>
<span id="L775" rel="#L775">775</span>
<span id="L776" rel="#L776">776</span>
<span id="L777" rel="#L777">777</span>
<span id="L778" rel="#L778">778</span>
<span id="L779" rel="#L779">779</span>
<span id="L780" rel="#L780">780</span>
<span id="L781" rel="#L781">781</span>
<span id="L782" rel="#L782">782</span>
<span id="L783" rel="#L783">783</span>
<span id="L784" rel="#L784">784</span>
<span id="L785" rel="#L785">785</span>
<span id="L786" rel="#L786">786</span>
<span id="L787" rel="#L787">787</span>
<span id="L788" rel="#L788">788</span>

            </td>
            <td class="blob-line-code">
                    <div class="highlight"><pre><div class='line' id='LC1'><span class="cm">/**</span></div><div class='line' id='LC2'><span class="cm"> * @preserve FastClick: polyfill to remove click delays on browsers with touch UIs.</span></div><div class='line' id='LC3'><span class="cm"> *</span></div><div class='line' id='LC4'><span class="cm"> * @version 0.6.11</span></div><div class='line' id='LC5'><span class="cm"> * @codingstandard ftlabs-jsv2</span></div><div class='line' id='LC6'><span class="cm"> * @copyright The Financial Times Limited [All Rights Reserved]</span></div><div class='line' id='LC7'><span class="cm"> * @license MIT License (see LICENSE.txt)</span></div><div class='line' id='LC8'><span class="cm"> */</span></div><div class='line' id='LC9'><br/></div><div class='line' id='LC10'><span class="cm">/*jslint browser:true, node:true*/</span></div><div class='line' id='LC11'><span class="cm">/*global define, Event, Node*/</span></div><div class='line' id='LC12'><br/></div><div class='line' id='LC13'><br/></div><div class='line' id='LC14'><span class="cm">/**</span></div><div class='line' id='LC15'><span class="cm"> * Instantiate fast-clicking listeners on the specificed layer.</span></div><div class='line' id='LC16'><span class="cm"> *</span></div><div class='line' id='LC17'><span class="cm"> * @constructor</span></div><div class='line' id='LC18'><span class="cm"> * @param {Element} layer The layer to listen on</span></div><div class='line' id='LC19'><span class="cm"> */</span></div><div class='line' id='LC20'><span class="kd">function</span> <span class="nx">FastClick</span><span class="p">(</span><span class="nx">layer</span><span class="p">)</span> <span class="p">{</span></div><div class='line' id='LC21'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="s1">&#39;use strict&#39;</span><span class="p">;</span></div><div class='line' id='LC22'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="kd">var</span> <span class="nx">oldOnClick</span><span class="p">,</span> <span class="nx">self</span> <span class="o">=</span> <span class="k">this</span><span class="p">;</span></div><div class='line' id='LC23'><br/></div><div class='line' id='LC24'><br/></div><div class='line' id='LC25'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="cm">/**</span></div><div class='line' id='LC26'><span class="cm">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; * Whether a click is currently being tracked.</span></div><div class='line' id='LC27'><span class="cm">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; *</span></div><div class='line' id='LC28'><span class="cm">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; * @type boolean</span></div><div class='line' id='LC29'><span class="cm">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; */</span></div><div class='line' id='LC30'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">this</span><span class="p">.</span><span class="nx">trackingClick</span> <span class="o">=</span> <span class="kc">false</span><span class="p">;</span></div><div class='line' id='LC31'><br/></div><div class='line' id='LC32'><br/></div><div class='line' id='LC33'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="cm">/**</span></div><div class='line' id='LC34'><span class="cm">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; * Timestamp for when when click tracking started.</span></div><div class='line' id='LC35'><span class="cm">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; *</span></div><div class='line' id='LC36'><span class="cm">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; * @type number</span></div><div class='line' id='LC37'><span class="cm">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; */</span></div><div class='line' id='LC38'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">this</span><span class="p">.</span><span class="nx">trackingClickStart</span> <span class="o">=</span> <span class="mi">0</span><span class="p">;</span></div><div class='line' id='LC39'><br/></div><div class='line' id='LC40'><br/></div><div class='line' id='LC41'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="cm">/**</span></div><div class='line' id='LC42'><span class="cm">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; * The element being tracked for a click.</span></div><div class='line' id='LC43'><span class="cm">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; *</span></div><div class='line' id='LC44'><span class="cm">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; * @type EventTarget</span></div><div class='line' id='LC45'><span class="cm">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; */</span></div><div class='line' id='LC46'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">this</span><span class="p">.</span><span class="nx">targetElement</span> <span class="o">=</span> <span class="kc">null</span><span class="p">;</span></div><div class='line' id='LC47'><br/></div><div class='line' id='LC48'><br/></div><div class='line' id='LC49'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="cm">/**</span></div><div class='line' id='LC50'><span class="cm">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; * X-coordinate of touch start event.</span></div><div class='line' id='LC51'><span class="cm">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; *</span></div><div class='line' id='LC52'><span class="cm">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; * @type number</span></div><div class='line' id='LC53'><span class="cm">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; */</span></div><div class='line' id='LC54'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">this</span><span class="p">.</span><span class="nx">touchStartX</span> <span class="o">=</span> <span class="mi">0</span><span class="p">;</span></div><div class='line' id='LC55'><br/></div><div class='line' id='LC56'><br/></div><div class='line' id='LC57'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="cm">/**</span></div><div class='line' id='LC58'><span class="cm">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; * Y-coordinate of touch start event.</span></div><div class='line' id='LC59'><span class="cm">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; *</span></div><div class='line' id='LC60'><span class="cm">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; * @type number</span></div><div class='line' id='LC61'><span class="cm">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; */</span></div><div class='line' id='LC62'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">this</span><span class="p">.</span><span class="nx">touchStartY</span> <span class="o">=</span> <span class="mi">0</span><span class="p">;</span></div><div class='line' id='LC63'><br/></div><div class='line' id='LC64'><br/></div><div class='line' id='LC65'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="cm">/**</span></div><div class='line' id='LC66'><span class="cm">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; * ID of the last touch, retrieved from Touch.identifier.</span></div><div class='line' id='LC67'><span class="cm">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; *</span></div><div class='line' id='LC68'><span class="cm">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; * @type number</span></div><div class='line' id='LC69'><span class="cm">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; */</span></div><div class='line' id='LC70'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">this</span><span class="p">.</span><span class="nx">lastTouchIdentifier</span> <span class="o">=</span> <span class="mi">0</span><span class="p">;</span></div><div class='line' id='LC71'><br/></div><div class='line' id='LC72'><br/></div><div class='line' id='LC73'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="cm">/**</span></div><div class='line' id='LC74'><span class="cm">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; * Touchmove boundary, beyond which a click will be cancelled.</span></div><div class='line' id='LC75'><span class="cm">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; *</span></div><div class='line' id='LC76'><span class="cm">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; * @type number</span></div><div class='line' id='LC77'><span class="cm">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; */</span></div><div class='line' id='LC78'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">this</span><span class="p">.</span><span class="nx">touchBoundary</span> <span class="o">=</span> <span class="mi">10</span><span class="p">;</span></div><div class='line' id='LC79'><br/></div><div class='line' id='LC80'><br/></div><div class='line' id='LC81'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="cm">/**</span></div><div class='line' id='LC82'><span class="cm">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; * The FastClick layer.</span></div><div class='line' id='LC83'><span class="cm">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; *</span></div><div class='line' id='LC84'><span class="cm">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; * @type Element</span></div><div class='line' id='LC85'><span class="cm">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; */</span></div><div class='line' id='LC86'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">this</span><span class="p">.</span><span class="nx">layer</span> <span class="o">=</span> <span class="nx">layer</span><span class="p">;</span></div><div class='line' id='LC87'><br/></div><div class='line' id='LC88'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">if</span> <span class="p">(</span><span class="o">!</span><span class="nx">layer</span> <span class="o">||</span> <span class="o">!</span><span class="nx">layer</span><span class="p">.</span><span class="nx">nodeType</span><span class="p">)</span> <span class="p">{</span></div><div class='line' id='LC89'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">throw</span> <span class="k">new</span> <span class="nx">TypeError</span><span class="p">(</span><span class="s1">&#39;Layer must be a document node&#39;</span><span class="p">);</span></div><div class='line' id='LC90'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">}</span></div><div class='line' id='LC91'><br/></div><div class='line' id='LC92'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="cm">/** @type function() */</span></div><div class='line' id='LC93'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">this</span><span class="p">.</span><span class="nx">onClick</span> <span class="o">=</span> <span class="kd">function</span><span class="p">()</span> <span class="p">{</span> <span class="k">return</span> <span class="nx">FastClick</span><span class="p">.</span><span class="nx">prototype</span><span class="p">.</span><span class="nx">onClick</span><span class="p">.</span><span class="nx">apply</span><span class="p">(</span><span class="nx">self</span><span class="p">,</span> <span class="nx">arguments</span><span class="p">);</span> <span class="p">};</span></div><div class='line' id='LC94'><br/></div><div class='line' id='LC95'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="cm">/** @type function() */</span></div><div class='line' id='LC96'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">this</span><span class="p">.</span><span class="nx">onMouse</span> <span class="o">=</span> <span class="kd">function</span><span class="p">()</span> <span class="p">{</span> <span class="k">return</span> <span class="nx">FastClick</span><span class="p">.</span><span class="nx">prototype</span><span class="p">.</span><span class="nx">onMouse</span><span class="p">.</span><span class="nx">apply</span><span class="p">(</span><span class="nx">self</span><span class="p">,</span> <span class="nx">arguments</span><span class="p">);</span> <span class="p">};</span></div><div class='line' id='LC97'><br/></div><div class='line' id='LC98'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="cm">/** @type function() */</span></div><div class='line' id='LC99'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">this</span><span class="p">.</span><span class="nx">onTouchStart</span> <span class="o">=</span> <span class="kd">function</span><span class="p">()</span> <span class="p">{</span> <span class="k">return</span> <span class="nx">FastClick</span><span class="p">.</span><span class="nx">prototype</span><span class="p">.</span><span class="nx">onTouchStart</span><span class="p">.</span><span class="nx">apply</span><span class="p">(</span><span class="nx">self</span><span class="p">,</span> <span class="nx">arguments</span><span class="p">);</span> <span class="p">};</span></div><div class='line' id='LC100'><br/></div><div class='line' id='LC101'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="cm">/** @type function() */</span></div><div class='line' id='LC102'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">this</span><span class="p">.</span><span class="nx">onTouchMove</span> <span class="o">=</span> <span class="kd">function</span><span class="p">()</span> <span class="p">{</span> <span class="k">return</span> <span class="nx">FastClick</span><span class="p">.</span><span class="nx">prototype</span><span class="p">.</span><span class="nx">onTouchMove</span><span class="p">.</span><span class="nx">apply</span><span class="p">(</span><span class="nx">self</span><span class="p">,</span> <span class="nx">arguments</span><span class="p">);</span> <span class="p">};</span></div><div class='line' id='LC103'><br/></div><div class='line' id='LC104'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="cm">/** @type function() */</span></div><div class='line' id='LC105'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">this</span><span class="p">.</span><span class="nx">onTouchEnd</span> <span class="o">=</span> <span class="kd">function</span><span class="p">()</span> <span class="p">{</span> <span class="k">return</span> <span class="nx">FastClick</span><span class="p">.</span><span class="nx">prototype</span><span class="p">.</span><span class="nx">onTouchEnd</span><span class="p">.</span><span class="nx">apply</span><span class="p">(</span><span class="nx">self</span><span class="p">,</span> <span class="nx">arguments</span><span class="p">);</span> <span class="p">};</span></div><div class='line' id='LC106'><br/></div><div class='line' id='LC107'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="cm">/** @type function() */</span></div><div class='line' id='LC108'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">this</span><span class="p">.</span><span class="nx">onTouchCancel</span> <span class="o">=</span> <span class="kd">function</span><span class="p">()</span> <span class="p">{</span> <span class="k">return</span> <span class="nx">FastClick</span><span class="p">.</span><span class="nx">prototype</span><span class="p">.</span><span class="nx">onTouchCancel</span><span class="p">.</span><span class="nx">apply</span><span class="p">(</span><span class="nx">self</span><span class="p">,</span> <span class="nx">arguments</span><span class="p">);</span> <span class="p">};</span></div><div class='line' id='LC109'><br/></div><div class='line' id='LC110'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">if</span> <span class="p">(</span><span class="nx">FastClick</span><span class="p">.</span><span class="nx">notNeeded</span><span class="p">(</span><span class="nx">layer</span><span class="p">))</span> <span class="p">{</span></div><div class='line' id='LC111'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">return</span><span class="p">;</span></div><div class='line' id='LC112'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">}</span></div><div class='line' id='LC113'><br/></div><div class='line' id='LC114'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">// Set up event handlers as required</span></div><div class='line' id='LC115'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">if</span> <span class="p">(</span><span class="k">this</span><span class="p">.</span><span class="nx">deviceIsAndroid</span><span class="p">)</span> <span class="p">{</span></div><div class='line' id='LC116'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nx">layer</span><span class="p">.</span><span class="nx">addEventListener</span><span class="p">(</span><span class="s1">&#39;mouseover&#39;</span><span class="p">,</span> <span class="k">this</span><span class="p">.</span><span class="nx">onMouse</span><span class="p">,</span> <span class="kc">true</span><span class="p">);</span></div><div class='line' id='LC117'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nx">layer</span><span class="p">.</span><span class="nx">addEventListener</span><span class="p">(</span><span class="s1">&#39;mousedown&#39;</span><span class="p">,</span> <span class="k">this</span><span class="p">.</span><span class="nx">onMouse</span><span class="p">,</span> <span class="kc">true</span><span class="p">);</span></div><div class='line' id='LC118'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nx">layer</span><span class="p">.</span><span class="nx">addEventListener</span><span class="p">(</span><span class="s1">&#39;mouseup&#39;</span><span class="p">,</span> <span class="k">this</span><span class="p">.</span><span class="nx">onMouse</span><span class="p">,</span> <span class="kc">true</span><span class="p">);</span></div><div class='line' id='LC119'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">}</span></div><div class='line' id='LC120'><br/></div><div class='line' id='LC121'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nx">layer</span><span class="p">.</span><span class="nx">addEventListener</span><span class="p">(</span><span class="s1">&#39;click&#39;</span><span class="p">,</span> <span class="k">this</span><span class="p">.</span><span class="nx">onClick</span><span class="p">,</span> <span class="kc">true</span><span class="p">);</span></div><div class='line' id='LC122'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nx">layer</span><span class="p">.</span><span class="nx">addEventListener</span><span class="p">(</span><span class="s1">&#39;touchstart&#39;</span><span class="p">,</span> <span class="k">this</span><span class="p">.</span><span class="nx">onTouchStart</span><span class="p">,</span> <span class="kc">false</span><span class="p">);</span></div><div class='line' id='LC123'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nx">layer</span><span class="p">.</span><span class="nx">addEventListener</span><span class="p">(</span><span class="s1">&#39;touchmove&#39;</span><span class="p">,</span> <span class="k">this</span><span class="p">.</span><span class="nx">onTouchMove</span><span class="p">,</span> <span class="kc">false</span><span class="p">);</span></div><div class='line' id='LC124'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nx">layer</span><span class="p">.</span><span class="nx">addEventListener</span><span class="p">(</span><span class="s1">&#39;touchend&#39;</span><span class="p">,</span> <span class="k">this</span><span class="p">.</span><span class="nx">onTouchEnd</span><span class="p">,</span> <span class="kc">false</span><span class="p">);</span></div><div class='line' id='LC125'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nx">layer</span><span class="p">.</span><span class="nx">addEventListener</span><span class="p">(</span><span class="s1">&#39;touchcancel&#39;</span><span class="p">,</span> <span class="k">this</span><span class="p">.</span><span class="nx">onTouchCancel</span><span class="p">,</span> <span class="kc">false</span><span class="p">);</span></div><div class='line' id='LC126'><br/></div><div class='line' id='LC127'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">// Hack is required for browsers that don&#39;t support Event#stopImmediatePropagation (e.g. Android 2)</span></div><div class='line' id='LC128'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">// which is how FastClick normally stops click events bubbling to callbacks registered on the FastClick</span></div><div class='line' id='LC129'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">// layer when they are cancelled.</span></div><div class='line' id='LC130'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">if</span> <span class="p">(</span><span class="o">!</span><span class="nx">Event</span><span class="p">.</span><span class="nx">prototype</span><span class="p">.</span><span class="nx">stopImmediatePropagation</span><span class="p">)</span> <span class="p">{</span></div><div class='line' id='LC131'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nx">layer</span><span class="p">.</span><span class="nx">removeEventListener</span> <span class="o">=</span> <span class="kd">function</span><span class="p">(</span><span class="nx">type</span><span class="p">,</span> <span class="nx">callback</span><span class="p">,</span> <span class="nx">capture</span><span class="p">)</span> <span class="p">{</span></div><div class='line' id='LC132'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="kd">var</span> <span class="nx">rmv</span> <span class="o">=</span> <span class="nx">Node</span><span class="p">.</span><span class="nx">prototype</span><span class="p">.</span><span class="nx">removeEventListener</span><span class="p">;</span></div><div class='line' id='LC133'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">if</span> <span class="p">(</span><span class="nx">type</span> <span class="o">===</span> <span class="s1">&#39;click&#39;</span><span class="p">)</span> <span class="p">{</span></div><div class='line' id='LC134'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nx">rmv</span><span class="p">.</span><span class="nx">call</span><span class="p">(</span><span class="nx">layer</span><span class="p">,</span> <span class="nx">type</span><span class="p">,</span> <span class="nx">callback</span><span class="p">.</span><span class="nx">hijacked</span> <span class="o">||</span> <span class="nx">callback</span><span class="p">,</span> <span class="nx">capture</span><span class="p">);</span></div><div class='line' id='LC135'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">}</span> <span class="k">else</span> <span class="p">{</span></div><div class='line' id='LC136'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nx">rmv</span><span class="p">.</span><span class="nx">call</span><span class="p">(</span><span class="nx">layer</span><span class="p">,</span> <span class="nx">type</span><span class="p">,</span> <span class="nx">callback</span><span class="p">,</span> <span class="nx">capture</span><span class="p">);</span></div><div class='line' id='LC137'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">}</span></div><div class='line' id='LC138'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">};</span></div><div class='line' id='LC139'><br/></div><div class='line' id='LC140'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nx">layer</span><span class="p">.</span><span class="nx">addEventListener</span> <span class="o">=</span> <span class="kd">function</span><span class="p">(</span><span class="nx">type</span><span class="p">,</span> <span class="nx">callback</span><span class="p">,</span> <span class="nx">capture</span><span class="p">)</span> <span class="p">{</span></div><div class='line' id='LC141'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="kd">var</span> <span class="nx">adv</span> <span class="o">=</span> <span class="nx">Node</span><span class="p">.</span><span class="nx">prototype</span><span class="p">.</span><span class="nx">addEventListener</span><span class="p">;</span></div><div class='line' id='LC142'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">if</span> <span class="p">(</span><span class="nx">type</span> <span class="o">===</span> <span class="s1">&#39;click&#39;</span><span class="p">)</span> <span class="p">{</span></div><div class='line' id='LC143'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nx">adv</span><span class="p">.</span><span class="nx">call</span><span class="p">(</span><span class="nx">layer</span><span class="p">,</span> <span class="nx">type</span><span class="p">,</span> <span class="nx">callback</span><span class="p">.</span><span class="nx">hijacked</span> <span class="o">||</span> <span class="p">(</span><span class="nx">callback</span><span class="p">.</span><span class="nx">hijacked</span> <span class="o">=</span> <span class="kd">function</span><span class="p">(</span><span class="nx">event</span><span class="p">)</span> <span class="p">{</span></div><div class='line' id='LC144'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">if</span> <span class="p">(</span><span class="o">!</span><span class="nx">event</span><span class="p">.</span><span class="nx">propagationStopped</span><span class="p">)</span> <span class="p">{</span></div><div class='line' id='LC145'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nx">callback</span><span class="p">(</span><span class="nx">event</span><span class="p">);</span></div><div class='line' id='LC146'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">}</span></div><div class='line' id='LC147'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">}),</span> <span class="nx">capture</span><span class="p">);</span></div><div class='line' id='LC148'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">}</span> <span class="k">else</span> <span class="p">{</span></div><div class='line' id='LC149'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nx">adv</span><span class="p">.</span><span class="nx">call</span><span class="p">(</span><span class="nx">layer</span><span class="p">,</span> <span class="nx">type</span><span class="p">,</span> <span class="nx">callback</span><span class="p">,</span> <span class="nx">capture</span><span class="p">);</span></div><div class='line' id='LC150'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">}</span></div><div class='line' id='LC151'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">};</span></div><div class='line' id='LC152'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">}</span></div><div class='line' id='LC153'><br/></div><div class='line' id='LC154'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">// If a handler is already declared in the element&#39;s onclick attribute, it will be fired before</span></div><div class='line' id='LC155'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">// FastClick&#39;s onClick handler. Fix this by pulling out the user-defined handler function and</span></div><div class='line' id='LC156'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">// adding it as listener.</span></div><div class='line' id='LC157'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">if</span> <span class="p">(</span><span class="k">typeof</span> <span class="nx">layer</span><span class="p">.</span><span class="nx">onclick</span> <span class="o">===</span> <span class="s1">&#39;function&#39;</span><span class="p">)</span> <span class="p">{</span></div><div class='line' id='LC158'><br/></div><div class='line' id='LC159'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">// Android browser on at least 3.2 requires a new reference to the function in layer.onclick</span></div><div class='line' id='LC160'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">// - the old one won&#39;t work if passed to addEventListener directly.</span></div><div class='line' id='LC161'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nx">oldOnClick</span> <span class="o">=</span> <span class="nx">layer</span><span class="p">.</span><span class="nx">onclick</span><span class="p">;</span></div><div class='line' id='LC162'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nx">layer</span><span class="p">.</span><span class="nx">addEventListener</span><span class="p">(</span><span class="s1">&#39;click&#39;</span><span class="p">,</span> <span class="kd">function</span><span class="p">(</span><span class="nx">event</span><span class="p">)</span> <span class="p">{</span></div><div class='line' id='LC163'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nx">oldOnClick</span><span class="p">(</span><span class="nx">event</span><span class="p">);</span></div><div class='line' id='LC164'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">},</span> <span class="kc">false</span><span class="p">);</span></div><div class='line' id='LC165'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nx">layer</span><span class="p">.</span><span class="nx">onclick</span> <span class="o">=</span> <span class="kc">null</span><span class="p">;</span></div><div class='line' id='LC166'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">}</span></div><div class='line' id='LC167'><span class="p">}</span></div><div class='line' id='LC168'><br/></div><div class='line' id='LC169'><br/></div><div class='line' id='LC170'><span class="cm">/**</span></div><div class='line' id='LC171'><span class="cm"> * Android requires exceptions.</span></div><div class='line' id='LC172'><span class="cm"> *</span></div><div class='line' id='LC173'><span class="cm"> * @type boolean</span></div><div class='line' id='LC174'><span class="cm"> */</span></div><div class='line' id='LC175'><span class="nx">FastClick</span><span class="p">.</span><span class="nx">prototype</span><span class="p">.</span><span class="nx">deviceIsAndroid</span> <span class="o">=</span> <span class="nx">navigator</span><span class="p">.</span><span class="nx">userAgent</span><span class="p">.</span><span class="nx">indexOf</span><span class="p">(</span><span class="s1">&#39;Android&#39;</span><span class="p">)</span> <span class="o">&gt;</span> <span class="mi">0</span><span class="p">;</span></div><div class='line' id='LC176'><br/></div><div class='line' id='LC177'><br/></div><div class='line' id='LC178'><span class="cm">/**</span></div><div class='line' id='LC179'><span class="cm"> * iOS requires exceptions.</span></div><div class='line' id='LC180'><span class="cm"> *</span></div><div class='line' id='LC181'><span class="cm"> * @type boolean</span></div><div class='line' id='LC182'><span class="cm"> */</span></div><div class='line' id='LC183'><span class="nx">FastClick</span><span class="p">.</span><span class="nx">prototype</span><span class="p">.</span><span class="nx">deviceIsIOS</span> <span class="o">=</span> <span class="sr">/iP(ad|hone|od)/</span><span class="p">.</span><span class="nx">test</span><span class="p">(</span><span class="nx">navigator</span><span class="p">.</span><span class="nx">userAgent</span><span class="p">);</span></div><div class='line' id='LC184'><br/></div><div class='line' id='LC185'><br/></div><div class='line' id='LC186'><span class="cm">/**</span></div><div class='line' id='LC187'><span class="cm"> * iOS 4 requires an exception for select elements.</span></div><div class='line' id='LC188'><span class="cm"> *</span></div><div class='line' id='LC189'><span class="cm"> * @type boolean</span></div><div class='line' id='LC190'><span class="cm"> */</span></div><div class='line' id='LC191'><span class="nx">FastClick</span><span class="p">.</span><span class="nx">prototype</span><span class="p">.</span><span class="nx">deviceIsIOS4</span> <span class="o">=</span> <span class="nx">FastClick</span><span class="p">.</span><span class="nx">prototype</span><span class="p">.</span><span class="nx">deviceIsIOS</span> <span class="o">&amp;&amp;</span> <span class="p">(</span><span class="sr">/OS 4_\d(_\d)?/</span><span class="p">).</span><span class="nx">test</span><span class="p">(</span><span class="nx">navigator</span><span class="p">.</span><span class="nx">userAgent</span><span class="p">);</span></div><div class='line' id='LC192'><br/></div><div class='line' id='LC193'><br/></div><div class='line' id='LC194'><span class="cm">/**</span></div><div class='line' id='LC195'><span class="cm"> * iOS 6.0(+?) requires the target element to be manually derived</span></div><div class='line' id='LC196'><span class="cm"> *</span></div><div class='line' id='LC197'><span class="cm"> * @type boolean</span></div><div class='line' id='LC198'><span class="cm"> */</span></div><div class='line' id='LC199'><span class="nx">FastClick</span><span class="p">.</span><span class="nx">prototype</span><span class="p">.</span><span class="nx">deviceIsIOSWithBadTarget</span> <span class="o">=</span> <span class="nx">FastClick</span><span class="p">.</span><span class="nx">prototype</span><span class="p">.</span><span class="nx">deviceIsIOS</span> <span class="o">&amp;&amp;</span> <span class="p">(</span><span class="sr">/OS ([6-9]|\d{2})_\d/</span><span class="p">).</span><span class="nx">test</span><span class="p">(</span><span class="nx">navigator</span><span class="p">.</span><span class="nx">userAgent</span><span class="p">);</span></div><div class='line' id='LC200'><br/></div><div class='line' id='LC201'><br/></div><div class='line' id='LC202'><span class="cm">/**</span></div><div class='line' id='LC203'><span class="cm"> * Determine whether a given element requires a native click.</span></div><div class='line' id='LC204'><span class="cm"> *</span></div><div class='line' id='LC205'><span class="cm"> * @param {EventTarget|Element} target Target DOM element</span></div><div class='line' id='LC206'><span class="cm"> * @returns {boolean} Returns true if the element needs a native click</span></div><div class='line' id='LC207'><span class="cm"> */</span></div><div class='line' id='LC208'><span class="nx">FastClick</span><span class="p">.</span><span class="nx">prototype</span><span class="p">.</span><span class="nx">needsClick</span> <span class="o">=</span> <span class="kd">function</span><span class="p">(</span><span class="nx">target</span><span class="p">)</span> <span class="p">{</span></div><div class='line' id='LC209'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="s1">&#39;use strict&#39;</span><span class="p">;</span></div><div class='line' id='LC210'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">switch</span> <span class="p">(</span><span class="nx">target</span><span class="p">.</span><span class="nx">nodeName</span><span class="p">.</span><span class="nx">toLowerCase</span><span class="p">())</span> <span class="p">{</span></div><div class='line' id='LC211'><br/></div><div class='line' id='LC212'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">// Don&#39;t send a synthetic click to disabled inputs (issue #62)</span></div><div class='line' id='LC213'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">case</span> <span class="s1">&#39;button&#39;</span><span class="o">:</span></div><div class='line' id='LC214'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">case</span> <span class="s1">&#39;select&#39;</span><span class="o">:</span></div><div class='line' id='LC215'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">case</span> <span class="s1">&#39;textarea&#39;</span><span class="o">:</span></div><div class='line' id='LC216'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">if</span> <span class="p">(</span><span class="nx">target</span><span class="p">.</span><span class="nx">disabled</span><span class="p">)</span> <span class="p">{</span></div><div class='line' id='LC217'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">return</span> <span class="kc">true</span><span class="p">;</span></div><div class='line' id='LC218'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">}</span></div><div class='line' id='LC219'><br/></div><div class='line' id='LC220'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">break</span><span class="p">;</span></div><div class='line' id='LC221'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">case</span> <span class="s1">&#39;input&#39;</span><span class="o">:</span></div><div class='line' id='LC222'><br/></div><div class='line' id='LC223'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">// File inputs need real clicks on iOS 6 due to a browser bug (issue #68)</span></div><div class='line' id='LC224'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">if</span> <span class="p">((</span><span class="k">this</span><span class="p">.</span><span class="nx">deviceIsIOS</span> <span class="o">&amp;&amp;</span> <span class="nx">target</span><span class="p">.</span><span class="nx">type</span> <span class="o">===</span> <span class="s1">&#39;file&#39;</span><span class="p">)</span> <span class="o">||</span> <span class="nx">target</span><span class="p">.</span><span class="nx">disabled</span><span class="p">)</span> <span class="p">{</span></div><div class='line' id='LC225'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">return</span> <span class="kc">true</span><span class="p">;</span></div><div class='line' id='LC226'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">}</span></div><div class='line' id='LC227'><br/></div><div class='line' id='LC228'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">break</span><span class="p">;</span></div><div class='line' id='LC229'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">case</span> <span class="s1">&#39;label&#39;</span><span class="o">:</span></div><div class='line' id='LC230'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">case</span> <span class="s1">&#39;video&#39;</span><span class="o">:</span></div><div class='line' id='LC231'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">return</span> <span class="kc">true</span><span class="p">;</span></div><div class='line' id='LC232'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">}</span></div><div class='line' id='LC233'><br/></div><div class='line' id='LC234'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">return</span> <span class="p">(</span><span class="sr">/\bneedsclick\b/</span><span class="p">).</span><span class="nx">test</span><span class="p">(</span><span class="nx">target</span><span class="p">.</span><span class="nx">className</span><span class="p">);</span></div><div class='line' id='LC235'><span class="p">};</span></div><div class='line' id='LC236'><br/></div><div class='line' id='LC237'><br/></div><div class='line' id='LC238'><span class="cm">/**</span></div><div class='line' id='LC239'><span class="cm"> * Determine whether a given element requires a call to focus to simulate click into element.</span></div><div class='line' id='LC240'><span class="cm"> *</span></div><div class='line' id='LC241'><span class="cm"> * @param {EventTarget|Element} target Target DOM element</span></div><div class='line' id='LC242'><span class="cm"> * @returns {boolean} Returns true if the element requires a call to focus to simulate native click.</span></div><div class='line' id='LC243'><span class="cm"> */</span></div><div class='line' id='LC244'><span class="nx">FastClick</span><span class="p">.</span><span class="nx">prototype</span><span class="p">.</span><span class="nx">needsFocus</span> <span class="o">=</span> <span class="kd">function</span><span class="p">(</span><span class="nx">target</span><span class="p">)</span> <span class="p">{</span></div><div class='line' id='LC245'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="s1">&#39;use strict&#39;</span><span class="p">;</span></div><div class='line' id='LC246'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">switch</span> <span class="p">(</span><span class="nx">target</span><span class="p">.</span><span class="nx">nodeName</span><span class="p">.</span><span class="nx">toLowerCase</span><span class="p">())</span> <span class="p">{</span></div><div class='line' id='LC247'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">case</span> <span class="s1">&#39;textarea&#39;</span><span class="o">:</span></div><div class='line' id='LC248'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">return</span> <span class="kc">true</span><span class="p">;</span></div><div class='line' id='LC249'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">case</span> <span class="s1">&#39;select&#39;</span><span class="o">:</span></div><div class='line' id='LC250'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">return</span> <span class="o">!</span><span class="k">this</span><span class="p">.</span><span class="nx">deviceIsAndroid</span><span class="p">;</span></div><div class='line' id='LC251'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">case</span> <span class="s1">&#39;input&#39;</span><span class="o">:</span></div><div class='line' id='LC252'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">switch</span> <span class="p">(</span><span class="nx">target</span><span class="p">.</span><span class="nx">type</span><span class="p">)</span> <span class="p">{</span></div><div class='line' id='LC253'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">case</span> <span class="s1">&#39;button&#39;</span><span class="o">:</span></div><div class='line' id='LC254'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">case</span> <span class="s1">&#39;checkbox&#39;</span><span class="o">:</span></div><div class='line' id='LC255'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">case</span> <span class="s1">&#39;file&#39;</span><span class="o">:</span></div><div class='line' id='LC256'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">case</span> <span class="s1">&#39;image&#39;</span><span class="o">:</span></div><div class='line' id='LC257'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">case</span> <span class="s1">&#39;radio&#39;</span><span class="o">:</span></div><div class='line' id='LC258'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">case</span> <span class="s1">&#39;submit&#39;</span><span class="o">:</span></div><div class='line' id='LC259'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">return</span> <span class="kc">false</span><span class="p">;</span></div><div class='line' id='LC260'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">}</span></div><div class='line' id='LC261'><br/></div><div class='line' id='LC262'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">// No point in attempting to focus disabled inputs</span></div><div class='line' id='LC263'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">return</span> <span class="o">!</span><span class="nx">target</span><span class="p">.</span><span class="nx">disabled</span> <span class="o">&amp;&amp;</span> <span class="o">!</span><span class="nx">target</span><span class="p">.</span><span class="nx">readOnly</span><span class="p">;</span></div><div class='line' id='LC264'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">default</span><span class="o">:</span></div><div class='line' id='LC265'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">return</span> <span class="p">(</span><span class="sr">/\bneedsfocus\b/</span><span class="p">).</span><span class="nx">test</span><span class="p">(</span><span class="nx">target</span><span class="p">.</span><span class="nx">className</span><span class="p">);</span></div><div class='line' id='LC266'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">}</span></div><div class='line' id='LC267'><span class="p">};</span></div><div class='line' id='LC268'><br/></div><div class='line' id='LC269'><br/></div><div class='line' id='LC270'><span class="cm">/**</span></div><div class='line' id='LC271'><span class="cm"> * Send a click event to the specified element.</span></div><div class='line' id='LC272'><span class="cm"> *</span></div><div class='line' id='LC273'><span class="cm"> * @param {EventTarget|Element} targetElement</span></div><div class='line' id='LC274'><span class="cm"> * @param {Event} event</span></div><div class='line' id='LC275'><span class="cm"> */</span></div><div class='line' id='LC276'><span class="nx">FastClick</span><span class="p">.</span><span class="nx">prototype</span><span class="p">.</span><span class="nx">sendClick</span> <span class="o">=</span> <span class="kd">function</span><span class="p">(</span><span class="nx">targetElement</span><span class="p">,</span> <span class="nx">event</span><span class="p">)</span> <span class="p">{</span></div><div class='line' id='LC277'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="s1">&#39;use strict&#39;</span><span class="p">;</span></div><div class='line' id='LC278'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="kd">var</span> <span class="nx">clickEvent</span><span class="p">,</span> <span class="nx">touch</span><span class="p">;</span></div><div class='line' id='LC279'><br/></div><div class='line' id='LC280'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">// On some Android devices activeElement needs to be blurred otherwise the synthetic click will have no effect (#24)</span></div><div class='line' id='LC281'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">if</span> <span class="p">(</span><span class="nb">document</span><span class="p">.</span><span class="nx">activeElement</span> <span class="o">&amp;&amp;</span> <span class="nb">document</span><span class="p">.</span><span class="nx">activeElement</span> <span class="o">!==</span> <span class="nx">targetElement</span><span class="p">)</span> <span class="p">{</span></div><div class='line' id='LC282'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nb">document</span><span class="p">.</span><span class="nx">activeElement</span><span class="p">.</span><span class="nx">blur</span><span class="p">();</span></div><div class='line' id='LC283'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">}</span></div><div class='line' id='LC284'><br/></div><div class='line' id='LC285'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nx">touch</span> <span class="o">=</span> <span class="nx">event</span><span class="p">.</span><span class="nx">changedTouches</span><span class="p">[</span><span class="mi">0</span><span class="p">];</span></div><div class='line' id='LC286'><br/></div><div class='line' id='LC287'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">// Synthesise a click event, with an extra attribute so it can be tracked</span></div><div class='line' id='LC288'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nx">clickEvent</span> <span class="o">=</span> <span class="nb">document</span><span class="p">.</span><span class="nx">createEvent</span><span class="p">(</span><span class="s1">&#39;MouseEvents&#39;</span><span class="p">);</span></div><div class='line' id='LC289'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nx">clickEvent</span><span class="p">.</span><span class="nx">initMouseEvent</span><span class="p">(</span><span class="k">this</span><span class="p">.</span><span class="nx">determineEventType</span><span class="p">(</span><span class="nx">targetElement</span><span class="p">),</span> <span class="kc">true</span><span class="p">,</span> <span class="kc">true</span><span class="p">,</span> <span class="nb">window</span><span class="p">,</span> <span class="mi">1</span><span class="p">,</span> <span class="nx">touch</span><span class="p">.</span><span class="nx">screenX</span><span class="p">,</span> <span class="nx">touch</span><span class="p">.</span><span class="nx">screenY</span><span class="p">,</span> <span class="nx">touch</span><span class="p">.</span><span class="nx">clientX</span><span class="p">,</span> <span class="nx">touch</span><span class="p">.</span><span class="nx">clientY</span><span class="p">,</span> <span class="kc">false</span><span class="p">,</span> <span class="kc">false</span><span class="p">,</span> <span class="kc">false</span><span class="p">,</span> <span class="kc">false</span><span class="p">,</span> <span class="mi">0</span><span class="p">,</span> <span class="kc">null</span><span class="p">);</span></div><div class='line' id='LC290'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nx">clickEvent</span><span class="p">.</span><span class="nx">forwardedTouchEvent</span> <span class="o">=</span> <span class="kc">true</span><span class="p">;</span></div><div class='line' id='LC291'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nx">targetElement</span><span class="p">.</span><span class="nx">dispatchEvent</span><span class="p">(</span><span class="nx">clickEvent</span><span class="p">);</span></div><div class='line' id='LC292'><span class="p">};</span></div><div class='line' id='LC293'><br/></div><div class='line' id='LC294'><span class="nx">FastClick</span><span class="p">.</span><span class="nx">prototype</span><span class="p">.</span><span class="nx">determineEventType</span> <span class="o">=</span> <span class="kd">function</span><span class="p">(</span><span class="nx">targetElement</span><span class="p">)</span> <span class="p">{</span></div><div class='line' id='LC295'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="s1">&#39;use strict&#39;</span><span class="p">;</span></div><div class='line' id='LC296'><br/></div><div class='line' id='LC297'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">//Issue #159: Android Chrome Select Box does not open with a synthetic click event</span></div><div class='line' id='LC298'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">if</span> <span class="p">(</span><span class="k">this</span><span class="p">.</span><span class="nx">deviceIsAndroid</span> <span class="o">&amp;&amp;</span> <span class="nx">targetElement</span><span class="p">.</span><span class="nx">tagName</span><span class="p">.</span><span class="nx">toLowerCase</span><span class="p">()</span> <span class="o">===</span> <span class="s1">&#39;select&#39;</span><span class="p">)</span> <span class="p">{</span></div><div class='line' id='LC299'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">return</span> <span class="s1">&#39;mousedown&#39;</span><span class="p">;</span></div><div class='line' id='LC300'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">}</span></div><div class='line' id='LC301'><br/></div><div class='line' id='LC302'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">return</span> <span class="s1">&#39;click&#39;</span><span class="p">;</span></div><div class='line' id='LC303'><span class="p">};</span></div><div class='line' id='LC304'><br/></div><div class='line' id='LC305'><br/></div><div class='line' id='LC306'><span class="cm">/**</span></div><div class='line' id='LC307'><span class="cm"> * @param {EventTarget|Element} targetElement</span></div><div class='line' id='LC308'><span class="cm"> */</span></div><div class='line' id='LC309'><span class="nx">FastClick</span><span class="p">.</span><span class="nx">prototype</span><span class="p">.</span><span class="nx">focus</span> <span class="o">=</span> <span class="kd">function</span><span class="p">(</span><span class="nx">targetElement</span><span class="p">)</span> <span class="p">{</span></div><div class='line' id='LC310'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="s1">&#39;use strict&#39;</span><span class="p">;</span></div><div class='line' id='LC311'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="kd">var</span> <span class="nx">length</span><span class="p">;</span></div><div class='line' id='LC312'><br/></div><div class='line' id='LC313'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">// Issue #160: on iOS 7, some input elements (e.g. date datetime) throw a vague TypeError on setSelectionRange. These elements don&#39;t have an integer value for the selectionStart and selectionEnd properties, but unfortunately that can&#39;t be used for detection because accessing the properties also throws a TypeError. Just check the type instead. Filed as Apple bug #15122724.</span></div><div class='line' id='LC314'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">if</span> <span class="p">(</span><span class="k">this</span><span class="p">.</span><span class="nx">deviceIsIOS</span> <span class="o">&amp;&amp;</span> <span class="nx">targetElement</span><span class="p">.</span><span class="nx">setSelectionRange</span> <span class="o">&amp;&amp;</span> <span class="nx">targetElement</span><span class="p">.</span><span class="nx">type</span><span class="p">.</span><span class="nx">indexOf</span><span class="p">(</span><span class="s1">&#39;date&#39;</span><span class="p">)</span> <span class="o">!==</span> <span class="mi">0</span> <span class="o">&amp;&amp;</span> <span class="nx">targetElement</span><span class="p">.</span><span class="nx">type</span> <span class="o">!==</span> <span class="s1">&#39;time&#39;</span><span class="p">)</span> <span class="p">{</span></div><div class='line' id='LC315'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nx">length</span> <span class="o">=</span> <span class="nx">targetElement</span><span class="p">.</span><span class="nx">value</span><span class="p">.</span><span class="nx">length</span><span class="p">;</span></div><div class='line' id='LC316'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nx">targetElement</span><span class="p">.</span><span class="nx">setSelectionRange</span><span class="p">(</span><span class="nx">length</span><span class="p">,</span> <span class="nx">length</span><span class="p">);</span></div><div class='line' id='LC317'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">}</span> <span class="k">else</span> <span class="p">{</span></div><div class='line' id='LC318'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nx">targetElement</span><span class="p">.</span><span class="nx">focus</span><span class="p">();</span></div><div class='line' id='LC319'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">}</span></div><div class='line' id='LC320'><span class="p">};</span></div><div class='line' id='LC321'><br/></div><div class='line' id='LC322'><br/></div><div class='line' id='LC323'><span class="cm">/**</span></div><div class='line' id='LC324'><span class="cm"> * Check whether the given target element is a child of a scrollable layer and if so, set a flag on it.</span></div><div class='line' id='LC325'><span class="cm"> *</span></div><div class='line' id='LC326'><span class="cm"> * @param {EventTarget|Element} targetElement</span></div><div class='line' id='LC327'><span class="cm"> */</span></div><div class='line' id='LC328'><span class="nx">FastClick</span><span class="p">.</span><span class="nx">prototype</span><span class="p">.</span><span class="nx">updateScrollParent</span> <span class="o">=</span> <span class="kd">function</span><span class="p">(</span><span class="nx">targetElement</span><span class="p">)</span> <span class="p">{</span></div><div class='line' id='LC329'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="s1">&#39;use strict&#39;</span><span class="p">;</span></div><div class='line' id='LC330'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="kd">var</span> <span class="nx">scrollParent</span><span class="p">,</span> <span class="nx">parentElement</span><span class="p">;</span></div><div class='line' id='LC331'><br/></div><div class='line' id='LC332'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nx">scrollParent</span> <span class="o">=</span> <span class="nx">targetElement</span><span class="p">.</span><span class="nx">fastClickScrollParent</span><span class="p">;</span></div><div class='line' id='LC333'><br/></div><div class='line' id='LC334'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">// Attempt to discover whether the target element is contained within a scrollable layer. Re-check if the</span></div><div class='line' id='LC335'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">// target element was moved to another parent.</span></div><div class='line' id='LC336'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">if</span> <span class="p">(</span><span class="o">!</span><span class="nx">scrollParent</span> <span class="o">||</span> <span class="o">!</span><span class="nx">scrollParent</span><span class="p">.</span><span class="nx">contains</span><span class="p">(</span><span class="nx">targetElement</span><span class="p">))</span> <span class="p">{</span></div><div class='line' id='LC337'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nx">parentElement</span> <span class="o">=</span> <span class="nx">targetElement</span><span class="p">;</span></div><div class='line' id='LC338'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">do</span> <span class="p">{</span></div><div class='line' id='LC339'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">if</span> <span class="p">(</span><span class="nx">parentElement</span><span class="p">.</span><span class="nx">scrollHeight</span> <span class="o">&gt;</span> <span class="nx">parentElement</span><span class="p">.</span><span class="nx">offsetHeight</span><span class="p">)</span> <span class="p">{</span></div><div class='line' id='LC340'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nx">scrollParent</span> <span class="o">=</span> <span class="nx">parentElement</span><span class="p">;</span></div><div class='line' id='LC341'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nx">targetElement</span><span class="p">.</span><span class="nx">fastClickScrollParent</span> <span class="o">=</span> <span class="nx">parentElement</span><span class="p">;</span></div><div class='line' id='LC342'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">break</span><span class="p">;</span></div><div class='line' id='LC343'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">}</span></div><div class='line' id='LC344'><br/></div><div class='line' id='LC345'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nx">parentElement</span> <span class="o">=</span> <span class="nx">parentElement</span><span class="p">.</span><span class="nx">parentElement</span><span class="p">;</span></div><div class='line' id='LC346'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">}</span> <span class="k">while</span> <span class="p">(</span><span class="nx">parentElement</span><span class="p">);</span></div><div class='line' id='LC347'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">}</span></div><div class='line' id='LC348'><br/></div><div class='line' id='LC349'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">// Always update the scroll top tracker if possible.</span></div><div class='line' id='LC350'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">if</span> <span class="p">(</span><span class="nx">scrollParent</span><span class="p">)</span> <span class="p">{</span></div><div class='line' id='LC351'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nx">scrollParent</span><span class="p">.</span><span class="nx">fastClickLastScrollTop</span> <span class="o">=</span> <span class="nx">scrollParent</span><span class="p">.</span><span class="nx">scrollTop</span><span class="p">;</span></div><div class='line' id='LC352'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">}</span></div><div class='line' id='LC353'><span class="p">};</span></div><div class='line' id='LC354'><br/></div><div class='line' id='LC355'><br/></div><div class='line' id='LC356'><span class="cm">/**</span></div><div class='line' id='LC357'><span class="cm"> * @param {EventTarget} targetElement</span></div><div class='line' id='LC358'><span class="cm"> * @returns {Element|EventTarget}</span></div><div class='line' id='LC359'><span class="cm"> */</span></div><div class='line' id='LC360'><span class="nx">FastClick</span><span class="p">.</span><span class="nx">prototype</span><span class="p">.</span><span class="nx">getTargetElementFromEventTarget</span> <span class="o">=</span> <span class="kd">function</span><span class="p">(</span><span class="nx">eventTarget</span><span class="p">)</span> <span class="p">{</span></div><div class='line' id='LC361'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="s1">&#39;use strict&#39;</span><span class="p">;</span></div><div class='line' id='LC362'><br/></div><div class='line' id='LC363'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">// On some older browsers (notably Safari on iOS 4.1 - see issue #56) the event target may be a text node.</span></div><div class='line' id='LC364'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">if</span> <span class="p">(</span><span class="nx">eventTarget</span><span class="p">.</span><span class="nx">nodeType</span> <span class="o">===</span> <span class="nx">Node</span><span class="p">.</span><span class="nx">TEXT_NODE</span><span class="p">)</span> <span class="p">{</span></div><div class='line' id='LC365'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">return</span> <span class="nx">eventTarget</span><span class="p">.</span><span class="nx">parentNode</span><span class="p">;</span></div><div class='line' id='LC366'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">}</span></div><div class='line' id='LC367'><br/></div><div class='line' id='LC368'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">return</span> <span class="nx">eventTarget</span><span class="p">;</span></div><div class='line' id='LC369'><span class="p">};</span></div><div class='line' id='LC370'><br/></div><div class='line' id='LC371'><br/></div><div class='line' id='LC372'><span class="cm">/**</span></div><div class='line' id='LC373'><span class="cm"> * On touch start, record the position and scroll offset.</span></div><div class='line' id='LC374'><span class="cm"> *</span></div><div class='line' id='LC375'><span class="cm"> * @param {Event} event</span></div><div class='line' id='LC376'><span class="cm"> * @returns {boolean}</span></div><div class='line' id='LC377'><span class="cm"> */</span></div><div class='line' id='LC378'><span class="nx">FastClick</span><span class="p">.</span><span class="nx">prototype</span><span class="p">.</span><span class="nx">onTouchStart</span> <span class="o">=</span> <span class="kd">function</span><span class="p">(</span><span class="nx">event</span><span class="p">)</span> <span class="p">{</span></div><div class='line' id='LC379'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="s1">&#39;use strict&#39;</span><span class="p">;</span></div><div class='line' id='LC380'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="kd">var</span> <span class="nx">targetElement</span><span class="p">,</span> <span class="nx">touch</span><span class="p">,</span> <span class="nx">selection</span><span class="p">;</span></div><div class='line' id='LC381'><br/></div><div class='line' id='LC382'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">// Ignore multiple touches, otherwise pinch-to-zoom is prevented if both fingers are on the FastClick element (issue #111).</span></div><div class='line' id='LC383'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">if</span> <span class="p">(</span><span class="nx">event</span><span class="p">.</span><span class="nx">targetTouches</span><span class="p">.</span><span class="nx">length</span> <span class="o">&gt;</span> <span class="mi">1</span><span class="p">)</span> <span class="p">{</span></div><div class='line' id='LC384'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">return</span> <span class="kc">true</span><span class="p">;</span></div><div class='line' id='LC385'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">}</span></div><div class='line' id='LC386'><br/></div><div class='line' id='LC387'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nx">targetElement</span> <span class="o">=</span> <span class="k">this</span><span class="p">.</span><span class="nx">getTargetElementFromEventTarget</span><span class="p">(</span><span class="nx">event</span><span class="p">.</span><span class="nx">target</span><span class="p">);</span></div><div class='line' id='LC388'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nx">touch</span> <span class="o">=</span> <span class="nx">event</span><span class="p">.</span><span class="nx">targetTouches</span><span class="p">[</span><span class="mi">0</span><span class="p">];</span></div><div class='line' id='LC389'><br/></div><div class='line' id='LC390'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">if</span> <span class="p">(</span><span class="k">this</span><span class="p">.</span><span class="nx">deviceIsIOS</span><span class="p">)</span> <span class="p">{</span></div><div class='line' id='LC391'><br/></div><div class='line' id='LC392'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">// Only trusted events will deselect text on iOS (issue #49)</span></div><div class='line' id='LC393'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nx">selection</span> <span class="o">=</span> <span class="nb">window</span><span class="p">.</span><span class="nx">getSelection</span><span class="p">();</span></div><div class='line' id='LC394'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">if</span> <span class="p">(</span><span class="nx">selection</span><span class="p">.</span><span class="nx">rangeCount</span> <span class="o">&amp;&amp;</span> <span class="o">!</span><span class="nx">selection</span><span class="p">.</span><span class="nx">isCollapsed</span><span class="p">)</span> <span class="p">{</span></div><div class='line' id='LC395'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">return</span> <span class="kc">true</span><span class="p">;</span></div><div class='line' id='LC396'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">}</span></div><div class='line' id='LC397'><br/></div><div class='line' id='LC398'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">if</span> <span class="p">(</span><span class="o">!</span><span class="k">this</span><span class="p">.</span><span class="nx">deviceIsIOS4</span><span class="p">)</span> <span class="p">{</span></div><div class='line' id='LC399'><br/></div><div class='line' id='LC400'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">// Weird things happen on iOS when an alert or confirm dialog is opened from a click event callback (issue #23):</span></div><div class='line' id='LC401'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">// when the user next taps anywhere else on the page, new touchstart and touchend events are dispatched</span></div><div class='line' id='LC402'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">// with the same identifier as the touch event that previously triggered the click that triggered the alert.</span></div><div class='line' id='LC403'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">// Sadly, there is an issue on iOS 4 that causes some normal touch events to have the same identifier as an</span></div><div class='line' id='LC404'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">// immediately preceeding touch event (issue #52), so this fix is unavailable on that platform.</span></div><div class='line' id='LC405'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">if</span> <span class="p">(</span><span class="nx">touch</span><span class="p">.</span><span class="nx">identifier</span> <span class="o">===</span> <span class="k">this</span><span class="p">.</span><span class="nx">lastTouchIdentifier</span><span class="p">)</span> <span class="p">{</span></div><div class='line' id='LC406'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nx">event</span><span class="p">.</span><span class="nx">preventDefault</span><span class="p">();</span></div><div class='line' id='LC407'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">return</span> <span class="kc">false</span><span class="p">;</span></div><div class='line' id='LC408'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">}</span></div><div class='line' id='LC409'><br/></div><div class='line' id='LC410'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">this</span><span class="p">.</span><span class="nx">lastTouchIdentifier</span> <span class="o">=</span> <span class="nx">touch</span><span class="p">.</span><span class="nx">identifier</span><span class="p">;</span></div><div class='line' id='LC411'><br/></div><div class='line' id='LC412'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">// If the target element is a child of a scrollable layer (using -webkit-overflow-scrolling: touch) and:</span></div><div class='line' id='LC413'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">// 1) the user does a fling scroll on the scrollable layer</span></div><div class='line' id='LC414'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">// 2) the user stops the fling scroll with another tap</span></div><div class='line' id='LC415'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">// then the event.target of the last &#39;touchend&#39; event will be the element that was under the user&#39;s finger</span></div><div class='line' id='LC416'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">// when the fling scroll was started, causing FastClick to send a click event to that layer - unless a check</span></div><div class='line' id='LC417'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">// is made to ensure that a parent layer was not scrolled before sending a synthetic click (issue #42).</span></div><div class='line' id='LC418'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">this</span><span class="p">.</span><span class="nx">updateScrollParent</span><span class="p">(</span><span class="nx">targetElement</span><span class="p">);</span></div><div class='line' id='LC419'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">}</span></div><div class='line' id='LC420'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">}</span></div><div class='line' id='LC421'><br/></div><div class='line' id='LC422'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">this</span><span class="p">.</span><span class="nx">trackingClick</span> <span class="o">=</span> <span class="kc">true</span><span class="p">;</span></div><div class='line' id='LC423'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">this</span><span class="p">.</span><span class="nx">trackingClickStart</span> <span class="o">=</span> <span class="nx">event</span><span class="p">.</span><span class="nx">timeStamp</span><span class="p">;</span></div><div class='line' id='LC424'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">this</span><span class="p">.</span><span class="nx">targetElement</span> <span class="o">=</span> <span class="nx">targetElement</span><span class="p">;</span></div><div class='line' id='LC425'><br/></div><div class='line' id='LC426'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">this</span><span class="p">.</span><span class="nx">touchStartX</span> <span class="o">=</span> <span class="nx">touch</span><span class="p">.</span><span class="nx">pageX</span><span class="p">;</span></div><div class='line' id='LC427'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">this</span><span class="p">.</span><span class="nx">touchStartY</span> <span class="o">=</span> <span class="nx">touch</span><span class="p">.</span><span class="nx">pageY</span><span class="p">;</span></div><div class='line' id='LC428'><br/></div><div class='line' id='LC429'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">// Prevent phantom clicks on fast double-tap (issue #36)</span></div><div class='line' id='LC430'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">if</span> <span class="p">((</span><span class="nx">event</span><span class="p">.</span><span class="nx">timeStamp</span> <span class="o">-</span> <span class="k">this</span><span class="p">.</span><span class="nx">lastClickTime</span><span class="p">)</span> <span class="o">&lt;</span> <span class="mi">200</span><span class="p">)</span> <span class="p">{</span></div><div class='line' id='LC431'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nx">event</span><span class="p">.</span><span class="nx">preventDefault</span><span class="p">();</span></div><div class='line' id='LC432'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">}</span></div><div class='line' id='LC433'><br/></div><div class='line' id='LC434'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">return</span> <span class="kc">true</span><span class="p">;</span></div><div class='line' id='LC435'><span class="p">};</span></div><div class='line' id='LC436'><br/></div><div class='line' id='LC437'><br/></div><div class='line' id='LC438'><span class="cm">/**</span></div><div class='line' id='LC439'><span class="cm"> * Based on a touchmove event object, check whether the touch has moved past a boundary since it started.</span></div><div class='line' id='LC440'><span class="cm"> *</span></div><div class='line' id='LC441'><span class="cm"> * @param {Event} event</span></div><div class='line' id='LC442'><span class="cm"> * @returns {boolean}</span></div><div class='line' id='LC443'><span class="cm"> */</span></div><div class='line' id='LC444'><span class="nx">FastClick</span><span class="p">.</span><span class="nx">prototype</span><span class="p">.</span><span class="nx">touchHasMoved</span> <span class="o">=</span> <span class="kd">function</span><span class="p">(</span><span class="nx">event</span><span class="p">)</span> <span class="p">{</span></div><div class='line' id='LC445'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="s1">&#39;use strict&#39;</span><span class="p">;</span></div><div class='line' id='LC446'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="kd">var</span> <span class="nx">touch</span> <span class="o">=</span> <span class="nx">event</span><span class="p">.</span><span class="nx">changedTouches</span><span class="p">[</span><span class="mi">0</span><span class="p">],</span> <span class="nx">boundary</span> <span class="o">=</span> <span class="k">this</span><span class="p">.</span><span class="nx">touchBoundary</span><span class="p">;</span></div><div class='line' id='LC447'><br/></div><div class='line' id='LC448'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">if</span> <span class="p">(</span><span class="nb">Math</span><span class="p">.</span><span class="nx">abs</span><span class="p">(</span><span class="nx">touch</span><span class="p">.</span><span class="nx">pageX</span> <span class="o">-</span> <span class="k">this</span><span class="p">.</span><span class="nx">touchStartX</span><span class="p">)</span> <span class="o">&gt;</span> <span class="nx">boundary</span> <span class="o">||</span> <span class="nb">Math</span><span class="p">.</span><span class="nx">abs</span><span class="p">(</span><span class="nx">touch</span><span class="p">.</span><span class="nx">pageY</span> <span class="o">-</span> <span class="k">this</span><span class="p">.</span><span class="nx">touchStartY</span><span class="p">)</span> <span class="o">&gt;</span> <span class="nx">boundary</span><span class="p">)</span> <span class="p">{</span></div><div class='line' id='LC449'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">return</span> <span class="kc">true</span><span class="p">;</span></div><div class='line' id='LC450'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">}</span></div><div class='line' id='LC451'><br/></div><div class='line' id='LC452'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">return</span> <span class="kc">false</span><span class="p">;</span></div><div class='line' id='LC453'><span class="p">};</span></div><div class='line' id='LC454'><br/></div><div class='line' id='LC455'><br/></div><div class='line' id='LC456'><span class="cm">/**</span></div><div class='line' id='LC457'><span class="cm"> * Update the last position.</span></div><div class='line' id='LC458'><span class="cm"> *</span></div><div class='line' id='LC459'><span class="cm"> * @param {Event} event</span></div><div class='line' id='LC460'><span class="cm"> * @returns {boolean}</span></div><div class='line' id='LC461'><span class="cm"> */</span></div><div class='line' id='LC462'><span class="nx">FastClick</span><span class="p">.</span><span class="nx">prototype</span><span class="p">.</span><span class="nx">onTouchMove</span> <span class="o">=</span> <span class="kd">function</span><span class="p">(</span><span class="nx">event</span><span class="p">)</span> <span class="p">{</span></div><div class='line' id='LC463'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="s1">&#39;use strict&#39;</span><span class="p">;</span></div><div class='line' id='LC464'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">if</span> <span class="p">(</span><span class="o">!</span><span class="k">this</span><span class="p">.</span><span class="nx">trackingClick</span><span class="p">)</span> <span class="p">{</span></div><div class='line' id='LC465'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">return</span> <span class="kc">true</span><span class="p">;</span></div><div class='line' id='LC466'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">}</span></div><div class='line' id='LC467'><br/></div><div class='line' id='LC468'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">// If the touch has moved, cancel the click tracking</span></div><div class='line' id='LC469'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">if</span> <span class="p">(</span><span class="k">this</span><span class="p">.</span><span class="nx">targetElement</span> <span class="o">!==</span> <span class="k">this</span><span class="p">.</span><span class="nx">getTargetElementFromEventTarget</span><span class="p">(</span><span class="nx">event</span><span class="p">.</span><span class="nx">target</span><span class="p">)</span> <span class="o">||</span> <span class="k">this</span><span class="p">.</span><span class="nx">touchHasMoved</span><span class="p">(</span><span class="nx">event</span><span class="p">))</span> <span class="p">{</span></div><div class='line' id='LC470'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">this</span><span class="p">.</span><span class="nx">trackingClick</span> <span class="o">=</span> <span class="kc">false</span><span class="p">;</span></div><div class='line' id='LC471'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">this</span><span class="p">.</span><span class="nx">targetElement</span> <span class="o">=</span> <span class="kc">null</span><span class="p">;</span></div><div class='line' id='LC472'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">}</span></div><div class='line' id='LC473'><br/></div><div class='line' id='LC474'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">return</span> <span class="kc">true</span><span class="p">;</span></div><div class='line' id='LC475'><span class="p">};</span></div><div class='line' id='LC476'><br/></div><div class='line' id='LC477'><br/></div><div class='line' id='LC478'><span class="cm">/**</span></div><div class='line' id='LC479'><span class="cm"> * Attempt to find the labelled control for the given label element.</span></div><div class='line' id='LC480'><span class="cm"> *</span></div><div class='line' id='LC481'><span class="cm"> * @param {EventTarget|HTMLLabelElement} labelElement</span></div><div class='line' id='LC482'><span class="cm"> * @returns {Element|null}</span></div><div class='line' id='LC483'><span class="cm"> */</span></div><div class='line' id='LC484'><span class="nx">FastClick</span><span class="p">.</span><span class="nx">prototype</span><span class="p">.</span><span class="nx">findControl</span> <span class="o">=</span> <span class="kd">function</span><span class="p">(</span><span class="nx">labelElement</span><span class="p">)</span> <span class="p">{</span></div><div class='line' id='LC485'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="s1">&#39;use strict&#39;</span><span class="p">;</span></div><div class='line' id='LC486'><br/></div><div class='line' id='LC487'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">// Fast path for newer browsers supporting the HTML5 control attribute</span></div><div class='line' id='LC488'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">if</span> <span class="p">(</span><span class="nx">labelElement</span><span class="p">.</span><span class="nx">control</span> <span class="o">!==</span> <span class="kc">undefined</span><span class="p">)</span> <span class="p">{</span></div><div class='line' id='LC489'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">return</span> <span class="nx">labelElement</span><span class="p">.</span><span class="nx">control</span><span class="p">;</span></div><div class='line' id='LC490'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">}</span></div><div class='line' id='LC491'><br/></div><div class='line' id='LC492'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">// All browsers under test that support touch events also support the HTML5 htmlFor attribute</span></div><div class='line' id='LC493'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">if</span> <span class="p">(</span><span class="nx">labelElement</span><span class="p">.</span><span class="nx">htmlFor</span><span class="p">)</span> <span class="p">{</span></div><div class='line' id='LC494'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">return</span> <span class="nb">document</span><span class="p">.</span><span class="nx">getElementById</span><span class="p">(</span><span class="nx">labelElement</span><span class="p">.</span><span class="nx">htmlFor</span><span class="p">);</span></div><div class='line' id='LC495'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">}</span></div><div class='line' id='LC496'><br/></div><div class='line' id='LC497'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">// If no for attribute exists, attempt to retrieve the first labellable descendant element</span></div><div class='line' id='LC498'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">// the list of which is defined here: http://www.w3.org/TR/html5/forms.html#category-label</span></div><div class='line' id='LC499'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">return</span> <span class="nx">labelElement</span><span class="p">.</span><span class="nx">querySelector</span><span class="p">(</span><span class="s1">&#39;button, input:not([type=hidden]), keygen, meter, output, progress, select, textarea&#39;</span><span class="p">);</span></div><div class='line' id='LC500'><span class="p">};</span></div><div class='line' id='LC501'><br/></div><div class='line' id='LC502'><br/></div><div class='line' id='LC503'><span class="cm">/**</span></div><div class='line' id='LC504'><span class="cm"> * On touch end, determine whether to send a click event at once.</span></div><div class='line' id='LC505'><span class="cm"> *</span></div><div class='line' id='LC506'><span class="cm"> * @param {Event} event</span></div><div class='line' id='LC507'><span class="cm"> * @returns {boolean}</span></div><div class='line' id='LC508'><span class="cm"> */</span></div><div class='line' id='LC509'><span class="nx">FastClick</span><span class="p">.</span><span class="nx">prototype</span><span class="p">.</span><span class="nx">onTouchEnd</span> <span class="o">=</span> <span class="kd">function</span><span class="p">(</span><span class="nx">event</span><span class="p">)</span> <span class="p">{</span></div><div class='line' id='LC510'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="s1">&#39;use strict&#39;</span><span class="p">;</span></div><div class='line' id='LC511'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="kd">var</span> <span class="nx">forElement</span><span class="p">,</span> <span class="nx">trackingClickStart</span><span class="p">,</span> <span class="nx">targetTagName</span><span class="p">,</span> <span class="nx">scrollParent</span><span class="p">,</span> <span class="nx">touch</span><span class="p">,</span> <span class="nx">targetElement</span> <span class="o">=</span> <span class="k">this</span><span class="p">.</span><span class="nx">targetElement</span><span class="p">;</span></div><div class='line' id='LC512'><br/></div><div class='line' id='LC513'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">if</span> <span class="p">(</span><span class="o">!</span><span class="k">this</span><span class="p">.</span><span class="nx">trackingClick</span><span class="p">)</span> <span class="p">{</span></div><div class='line' id='LC514'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">return</span> <span class="kc">true</span><span class="p">;</span></div><div class='line' id='LC515'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">}</span></div><div class='line' id='LC516'><br/></div><div class='line' id='LC517'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">// Prevent phantom clicks on fast double-tap (issue #36)</span></div><div class='line' id='LC518'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">if</span> <span class="p">((</span><span class="nx">event</span><span class="p">.</span><span class="nx">timeStamp</span> <span class="o">-</span> <span class="k">this</span><span class="p">.</span><span class="nx">lastClickTime</span><span class="p">)</span> <span class="o">&lt;</span> <span class="mi">200</span><span class="p">)</span> <span class="p">{</span></div><div class='line' id='LC519'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">this</span><span class="p">.</span><span class="nx">cancelNextClick</span> <span class="o">=</span> <span class="kc">true</span><span class="p">;</span></div><div class='line' id='LC520'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">return</span> <span class="kc">true</span><span class="p">;</span></div><div class='line' id='LC521'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">}</span></div><div class='line' id='LC522'><br/></div><div class='line' id='LC523'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">// Reset to prevent wrong click cancel on input (issue #156).</span></div><div class='line' id='LC524'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">this</span><span class="p">.</span><span class="nx">cancelNextClick</span> <span class="o">=</span> <span class="kc">false</span><span class="p">;</span></div><div class='line' id='LC525'><br/></div><div class='line' id='LC526'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">this</span><span class="p">.</span><span class="nx">lastClickTime</span> <span class="o">=</span> <span class="nx">event</span><span class="p">.</span><span class="nx">timeStamp</span><span class="p">;</span></div><div class='line' id='LC527'><br/></div><div class='line' id='LC528'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nx">trackingClickStart</span> <span class="o">=</span> <span class="k">this</span><span class="p">.</span><span class="nx">trackingClickStart</span><span class="p">;</span></div><div class='line' id='LC529'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">this</span><span class="p">.</span><span class="nx">trackingClick</span> <span class="o">=</span> <span class="kc">false</span><span class="p">;</span></div><div class='line' id='LC530'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">this</span><span class="p">.</span><span class="nx">trackingClickStart</span> <span class="o">=</span> <span class="mi">0</span><span class="p">;</span></div><div class='line' id='LC531'><br/></div><div class='line' id='LC532'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">// On some iOS devices, the targetElement supplied with the event is invalid if the layer</span></div><div class='line' id='LC533'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">// is performing a transition or scroll, and has to be re-detected manually. Note that</span></div><div class='line' id='LC534'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">// for this to function correctly, it must be called *after* the event target is checked!</span></div><div class='line' id='LC535'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">// See issue #57; also filed as rdar://13048589 .</span></div><div class='line' id='LC536'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">if</span> <span class="p">(</span><span class="k">this</span><span class="p">.</span><span class="nx">deviceIsIOSWithBadTarget</span><span class="p">)</span> <span class="p">{</span></div><div class='line' id='LC537'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nx">touch</span> <span class="o">=</span> <span class="nx">event</span><span class="p">.</span><span class="nx">changedTouches</span><span class="p">[</span><span class="mi">0</span><span class="p">];</span></div><div class='line' id='LC538'><br/></div><div class='line' id='LC539'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">// In certain cases arguments of elementFromPoint can be negative, so prevent setting targetElement to null</span></div><div class='line' id='LC540'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nx">targetElement</span> <span class="o">=</span> <span class="nb">document</span><span class="p">.</span><span class="nx">elementFromPoint</span><span class="p">(</span><span class="nx">touch</span><span class="p">.</span><span class="nx">pageX</span> <span class="o">-</span> <span class="nb">window</span><span class="p">.</span><span class="nx">pageXOffset</span><span class="p">,</span> <span class="nx">touch</span><span class="p">.</span><span class="nx">pageY</span> <span class="o">-</span> <span class="nb">window</span><span class="p">.</span><span class="nx">pageYOffset</span><span class="p">)</span> <span class="o">||</span> <span class="nx">targetElement</span><span class="p">;</span></div><div class='line' id='LC541'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nx">targetElement</span><span class="p">.</span><span class="nx">fastClickScrollParent</span> <span class="o">=</span> <span class="k">this</span><span class="p">.</span><span class="nx">targetElement</span><span class="p">.</span><span class="nx">fastClickScrollParent</span><span class="p">;</span></div><div class='line' id='LC542'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">}</span></div><div class='line' id='LC543'><br/></div><div class='line' id='LC544'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nx">targetTagName</span> <span class="o">=</span> <span class="nx">targetElement</span><span class="p">.</span><span class="nx">tagName</span><span class="p">.</span><span class="nx">toLowerCase</span><span class="p">();</span></div><div class='line' id='LC545'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">if</span> <span class="p">(</span><span class="nx">targetTagName</span> <span class="o">===</span> <span class="s1">&#39;label&#39;</span><span class="p">)</span> <span class="p">{</span></div><div class='line' id='LC546'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nx">forElement</span> <span class="o">=</span> <span class="k">this</span><span class="p">.</span><span class="nx">findControl</span><span class="p">(</span><span class="nx">targetElement</span><span class="p">);</span></div><div class='line' id='LC547'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">if</span> <span class="p">(</span><span class="nx">forElement</span><span class="p">)</span> <span class="p">{</span></div><div class='line' id='LC548'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">this</span><span class="p">.</span><span class="nx">focus</span><span class="p">(</span><span class="nx">targetElement</span><span class="p">);</span></div><div class='line' id='LC549'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">if</span> <span class="p">(</span><span class="k">this</span><span class="p">.</span><span class="nx">deviceIsAndroid</span><span class="p">)</span> <span class="p">{</span></div><div class='line' id='LC550'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">return</span> <span class="kc">false</span><span class="p">;</span></div><div class='line' id='LC551'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">}</span></div><div class='line' id='LC552'><br/></div><div class='line' id='LC553'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nx">targetElement</span> <span class="o">=</span> <span class="nx">forElement</span><span class="p">;</span></div><div class='line' id='LC554'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">}</span></div><div class='line' id='LC555'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">}</span> <span class="k">else</span> <span class="k">if</span> <span class="p">(</span><span class="k">this</span><span class="p">.</span><span class="nx">needsFocus</span><span class="p">(</span><span class="nx">targetElement</span><span class="p">))</span> <span class="p">{</span></div><div class='line' id='LC556'><br/></div><div class='line' id='LC557'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">// Case 1: If the touch started a while ago (best guess is 100ms based on tests for issue #36) then focus will be triggered anyway. Return early and unset the target element reference so that the subsequent click will be allowed through.</span></div><div class='line' id='LC558'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">// Case 2: Without this exception for input elements tapped when the document is contained in an iframe, then any inputted text won&#39;t be visible even though the value attribute is updated as the user types (issue #37).</span></div><div class='line' id='LC559'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">if</span> <span class="p">((</span><span class="nx">event</span><span class="p">.</span><span class="nx">timeStamp</span> <span class="o">-</span> <span class="nx">trackingClickStart</span><span class="p">)</span> <span class="o">&gt;</span> <span class="mi">100</span> <span class="o">||</span> <span class="p">(</span><span class="k">this</span><span class="p">.</span><span class="nx">deviceIsIOS</span> <span class="o">&amp;&amp;</span> <span class="nb">window</span><span class="p">.</span><span class="nx">top</span> <span class="o">!==</span> <span class="nb">window</span> <span class="o">&amp;&amp;</span> <span class="nx">targetTagName</span> <span class="o">===</span> <span class="s1">&#39;input&#39;</span><span class="p">))</span> <span class="p">{</span></div><div class='line' id='LC560'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">this</span><span class="p">.</span><span class="nx">targetElement</span> <span class="o">=</span> <span class="kc">null</span><span class="p">;</span></div><div class='line' id='LC561'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">return</span> <span class="kc">false</span><span class="p">;</span></div><div class='line' id='LC562'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">}</span></div><div class='line' id='LC563'><br/></div><div class='line' id='LC564'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">this</span><span class="p">.</span><span class="nx">focus</span><span class="p">(</span><span class="nx">targetElement</span><span class="p">);</span></div><div class='line' id='LC565'><br/></div><div class='line' id='LC566'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">// Select elements need the event to go through on iOS 4, otherwise the selector menu won&#39;t open.</span></div><div class='line' id='LC567'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">if</span> <span class="p">(</span><span class="o">!</span><span class="k">this</span><span class="p">.</span><span class="nx">deviceIsIOS4</span> <span class="o">||</span> <span class="nx">targetTagName</span> <span class="o">!==</span> <span class="s1">&#39;select&#39;</span><span class="p">)</span> <span class="p">{</span></div><div class='line' id='LC568'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">this</span><span class="p">.</span><span class="nx">targetElement</span> <span class="o">=</span> <span class="kc">null</span><span class="p">;</span></div><div class='line' id='LC569'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nx">event</span><span class="p">.</span><span class="nx">preventDefault</span><span class="p">();</span></div><div class='line' id='LC570'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">}</span></div><div class='line' id='LC571'><br/></div><div class='line' id='LC572'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">return</span> <span class="kc">false</span><span class="p">;</span></div><div class='line' id='LC573'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">}</span></div><div class='line' id='LC574'><br/></div><div class='line' id='LC575'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">if</span> <span class="p">(</span><span class="k">this</span><span class="p">.</span><span class="nx">deviceIsIOS</span> <span class="o">&amp;&amp;</span> <span class="o">!</span><span class="k">this</span><span class="p">.</span><span class="nx">deviceIsIOS4</span><span class="p">)</span> <span class="p">{</span></div><div class='line' id='LC576'><br/></div><div class='line' id='LC577'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">// Don&#39;t send a synthetic click event if the target element is contained within a parent layer that was scrolled</span></div><div class='line' id='LC578'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">// and this tap is being used to stop the scrolling (usually initiated by a fling - issue #42).</span></div><div class='line' id='LC579'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nx">scrollParent</span> <span class="o">=</span> <span class="nx">targetElement</span><span class="p">.</span><span class="nx">fastClickScrollParent</span><span class="p">;</span></div><div class='line' id='LC580'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">if</span> <span class="p">(</span><span class="nx">scrollParent</span> <span class="o">&amp;&amp;</span> <span class="nx">scrollParent</span><span class="p">.</span><span class="nx">fastClickLastScrollTop</span> <span class="o">!==</span> <span class="nx">scrollParent</span><span class="p">.</span><span class="nx">scrollTop</span><span class="p">)</span> <span class="p">{</span></div><div class='line' id='LC581'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">return</span> <span class="kc">true</span><span class="p">;</span></div><div class='line' id='LC582'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">}</span></div><div class='line' id='LC583'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">}</span></div><div class='line' id='LC584'><br/></div><div class='line' id='LC585'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">// Prevent the actual click from going though - unless the target node is marked as requiring</span></div><div class='line' id='LC586'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">// real clicks or if it is in the whitelist in which case only non-programmatic clicks are permitted.</span></div><div class='line' id='LC587'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">if</span> <span class="p">(</span><span class="o">!</span><span class="k">this</span><span class="p">.</span><span class="nx">needsClick</span><span class="p">(</span><span class="nx">targetElement</span><span class="p">))</span> <span class="p">{</span></div><div class='line' id='LC588'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nx">event</span><span class="p">.</span><span class="nx">preventDefault</span><span class="p">();</span></div><div class='line' id='LC589'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">this</span><span class="p">.</span><span class="nx">sendClick</span><span class="p">(</span><span class="nx">targetElement</span><span class="p">,</span> <span class="nx">event</span><span class="p">);</span></div><div class='line' id='LC590'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">}</span></div><div class='line' id='LC591'><br/></div><div class='line' id='LC592'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">return</span> <span class="kc">false</span><span class="p">;</span></div><div class='line' id='LC593'><span class="p">};</span></div><div class='line' id='LC594'><br/></div><div class='line' id='LC595'><br/></div><div class='line' id='LC596'><span class="cm">/**</span></div><div class='line' id='LC597'><span class="cm"> * On touch cancel, stop tracking the click.</span></div><div class='line' id='LC598'><span class="cm"> *</span></div><div class='line' id='LC599'><span class="cm"> * @returns {void}</span></div><div class='line' id='LC600'><span class="cm"> */</span></div><div class='line' id='LC601'><span class="nx">FastClick</span><span class="p">.</span><span class="nx">prototype</span><span class="p">.</span><span class="nx">onTouchCancel</span> <span class="o">=</span> <span class="kd">function</span><span class="p">()</span> <span class="p">{</span></div><div class='line' id='LC602'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="s1">&#39;use strict&#39;</span><span class="p">;</span></div><div class='line' id='LC603'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">this</span><span class="p">.</span><span class="nx">trackingClick</span> <span class="o">=</span> <span class="kc">false</span><span class="p">;</span></div><div class='line' id='LC604'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">this</span><span class="p">.</span><span class="nx">targetElement</span> <span class="o">=</span> <span class="kc">null</span><span class="p">;</span></div><div class='line' id='LC605'><span class="p">};</span></div><div class='line' id='LC606'><br/></div><div class='line' id='LC607'><br/></div><div class='line' id='LC608'><span class="cm">/**</span></div><div class='line' id='LC609'><span class="cm"> * Determine mouse events which should be permitted.</span></div><div class='line' id='LC610'><span class="cm"> *</span></div><div class='line' id='LC611'><span class="cm"> * @param {Event} event</span></div><div class='line' id='LC612'><span class="cm"> * @returns {boolean}</span></div><div class='line' id='LC613'><span class="cm"> */</span></div><div class='line' id='LC614'><span class="nx">FastClick</span><span class="p">.</span><span class="nx">prototype</span><span class="p">.</span><span class="nx">onMouse</span> <span class="o">=</span> <span class="kd">function</span><span class="p">(</span><span class="nx">event</span><span class="p">)</span> <span class="p">{</span></div><div class='line' id='LC615'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="s1">&#39;use strict&#39;</span><span class="p">;</span></div><div class='line' id='LC616'><br/></div><div class='line' id='LC617'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">// If a target element was never set (because a touch event was never fired) allow the event</span></div><div class='line' id='LC618'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">if</span> <span class="p">(</span><span class="o">!</span><span class="k">this</span><span class="p">.</span><span class="nx">targetElement</span><span class="p">)</span> <span class="p">{</span></div><div class='line' id='LC619'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">return</span> <span class="kc">true</span><span class="p">;</span></div><div class='line' id='LC620'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">}</span></div><div class='line' id='LC621'><br/></div><div class='line' id='LC622'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">if</span> <span class="p">(</span><span class="nx">event</span><span class="p">.</span><span class="nx">forwardedTouchEvent</span><span class="p">)</span> <span class="p">{</span></div><div class='line' id='LC623'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">return</span> <span class="kc">true</span><span class="p">;</span></div><div class='line' id='LC624'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">}</span></div><div class='line' id='LC625'><br/></div><div class='line' id='LC626'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">// Programmatically generated events targeting a specific element should be permitted</span></div><div class='line' id='LC627'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">if</span> <span class="p">(</span><span class="o">!</span><span class="nx">event</span><span class="p">.</span><span class="nx">cancelable</span><span class="p">)</span> <span class="p">{</span></div><div class='line' id='LC628'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">return</span> <span class="kc">true</span><span class="p">;</span></div><div class='line' id='LC629'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">}</span></div><div class='line' id='LC630'><br/></div><div class='line' id='LC631'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">// Derive and check the target element to see whether the mouse event needs to be permitted;</span></div><div class='line' id='LC632'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">// unless explicitly enabled, prevent non-touch click events from triggering actions,</span></div><div class='line' id='LC633'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">// to prevent ghost/doubleclicks.</span></div><div class='line' id='LC634'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">if</span> <span class="p">(</span><span class="o">!</span><span class="k">this</span><span class="p">.</span><span class="nx">needsClick</span><span class="p">(</span><span class="k">this</span><span class="p">.</span><span class="nx">targetElement</span><span class="p">)</span> <span class="o">||</span> <span class="k">this</span><span class="p">.</span><span class="nx">cancelNextClick</span><span class="p">)</span> <span class="p">{</span></div><div class='line' id='LC635'><br/></div><div class='line' id='LC636'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">// Prevent any user-added listeners declared on FastClick element from being fired.</span></div><div class='line' id='LC637'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">if</span> <span class="p">(</span><span class="nx">event</span><span class="p">.</span><span class="nx">stopImmediatePropagation</span><span class="p">)</span> <span class="p">{</span></div><div class='line' id='LC638'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nx">event</span><span class="p">.</span><span class="nx">stopImmediatePropagation</span><span class="p">();</span></div><div class='line' id='LC639'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">}</span> <span class="k">else</span> <span class="p">{</span></div><div class='line' id='LC640'><br/></div><div class='line' id='LC641'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">// Part of the hack for browsers that don&#39;t support Event#stopImmediatePropagation (e.g. Android 2)</span></div><div class='line' id='LC642'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nx">event</span><span class="p">.</span><span class="nx">propagationStopped</span> <span class="o">=</span> <span class="kc">true</span><span class="p">;</span></div><div class='line' id='LC643'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">}</span></div><div class='line' id='LC644'><br/></div><div class='line' id='LC645'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">// Cancel the event</span></div><div class='line' id='LC646'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nx">event</span><span class="p">.</span><span class="nx">stopPropagation</span><span class="p">();</span></div><div class='line' id='LC647'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nx">event</span><span class="p">.</span><span class="nx">preventDefault</span><span class="p">();</span></div><div class='line' id='LC648'><br/></div><div class='line' id='LC649'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">return</span> <span class="kc">false</span><span class="p">;</span></div><div class='line' id='LC650'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">}</span></div><div class='line' id='LC651'><br/></div><div class='line' id='LC652'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">// If the mouse event is permitted, return true for the action to go through.</span></div><div class='line' id='LC653'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">return</span> <span class="kc">true</span><span class="p">;</span></div><div class='line' id='LC654'><span class="p">};</span></div><div class='line' id='LC655'><br/></div><div class='line' id='LC656'><br/></div><div class='line' id='LC657'><span class="cm">/**</span></div><div class='line' id='LC658'><span class="cm"> * On actual clicks, determine whether this is a touch-generated click, a click action occurring</span></div><div class='line' id='LC659'><span class="cm"> * naturally after a delay after a touch (which needs to be cancelled to avoid duplication), or</span></div><div class='line' id='LC660'><span class="cm"> * an actual click which should be permitted.</span></div><div class='line' id='LC661'><span class="cm"> *</span></div><div class='line' id='LC662'><span class="cm"> * @param {Event} event</span></div><div class='line' id='LC663'><span class="cm"> * @returns {boolean}</span></div><div class='line' id='LC664'><span class="cm"> */</span></div><div class='line' id='LC665'><span class="nx">FastClick</span><span class="p">.</span><span class="nx">prototype</span><span class="p">.</span><span class="nx">onClick</span> <span class="o">=</span> <span class="kd">function</span><span class="p">(</span><span class="nx">event</span><span class="p">)</span> <span class="p">{</span></div><div class='line' id='LC666'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="s1">&#39;use strict&#39;</span><span class="p">;</span></div><div class='line' id='LC667'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="kd">var</span> <span class="nx">permitted</span><span class="p">;</span></div><div class='line' id='LC668'><br/></div><div class='line' id='LC669'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">// It&#39;s possible for another FastClick-like library delivered with third-party code to fire a click event before FastClick does (issue #44). In that case, set the click-tracking flag back to false and return early. This will cause onTouchEnd to return early.</span></div><div class='line' id='LC670'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">if</span> <span class="p">(</span><span class="k">this</span><span class="p">.</span><span class="nx">trackingClick</span><span class="p">)</span> <span class="p">{</span></div><div class='line' id='LC671'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">this</span><span class="p">.</span><span class="nx">targetElement</span> <span class="o">=</span> <span class="kc">null</span><span class="p">;</span></div><div class='line' id='LC672'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">this</span><span class="p">.</span><span class="nx">trackingClick</span> <span class="o">=</span> <span class="kc">false</span><span class="p">;</span></div><div class='line' id='LC673'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">return</span> <span class="kc">true</span><span class="p">;</span></div><div class='line' id='LC674'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">}</span></div><div class='line' id='LC675'><br/></div><div class='line' id='LC676'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">// Very odd behaviour on iOS (issue #18): if a submit element is present inside a form and the user hits enter in the iOS simulator or clicks the Go button on the pop-up OS keyboard the a kind of &#39;fake&#39; click event will be triggered with the submit-type input element as the target.</span></div><div class='line' id='LC677'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">if</span> <span class="p">(</span><span class="nx">event</span><span class="p">.</span><span class="nx">target</span><span class="p">.</span><span class="nx">type</span> <span class="o">===</span> <span class="s1">&#39;submit&#39;</span> <span class="o">&amp;&amp;</span> <span class="nx">event</span><span class="p">.</span><span class="nx">detail</span> <span class="o">===</span> <span class="mi">0</span><span class="p">)</span> <span class="p">{</span></div><div class='line' id='LC678'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">return</span> <span class="kc">true</span><span class="p">;</span></div><div class='line' id='LC679'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">}</span></div><div class='line' id='LC680'><br/></div><div class='line' id='LC681'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nx">permitted</span> <span class="o">=</span> <span class="k">this</span><span class="p">.</span><span class="nx">onMouse</span><span class="p">(</span><span class="nx">event</span><span class="p">);</span></div><div class='line' id='LC682'><br/></div><div class='line' id='LC683'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">// Only unset targetElement if the click is not permitted. This will ensure that the check for !targetElement in onMouse fails and the browser&#39;s click doesn&#39;t go through.</span></div><div class='line' id='LC684'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">if</span> <span class="p">(</span><span class="o">!</span><span class="nx">permitted</span><span class="p">)</span> <span class="p">{</span></div><div class='line' id='LC685'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">this</span><span class="p">.</span><span class="nx">targetElement</span> <span class="o">=</span> <span class="kc">null</span><span class="p">;</span></div><div class='line' id='LC686'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">}</span></div><div class='line' id='LC687'><br/></div><div class='line' id='LC688'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">// If clicks are permitted, return true for the action to go through.</span></div><div class='line' id='LC689'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">return</span> <span class="nx">permitted</span><span class="p">;</span></div><div class='line' id='LC690'><span class="p">};</span></div><div class='line' id='LC691'><br/></div><div class='line' id='LC692'><br/></div><div class='line' id='LC693'><span class="cm">/**</span></div><div class='line' id='LC694'><span class="cm"> * Remove all FastClick&#39;s event listeners.</span></div><div class='line' id='LC695'><span class="cm"> *</span></div><div class='line' id='LC696'><span class="cm"> * @returns {void}</span></div><div class='line' id='LC697'><span class="cm"> */</span></div><div class='line' id='LC698'><span class="nx">FastClick</span><span class="p">.</span><span class="nx">prototype</span><span class="p">.</span><span class="nx">destroy</span> <span class="o">=</span> <span class="kd">function</span><span class="p">()</span> <span class="p">{</span></div><div class='line' id='LC699'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="s1">&#39;use strict&#39;</span><span class="p">;</span></div><div class='line' id='LC700'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="kd">var</span> <span class="nx">layer</span> <span class="o">=</span> <span class="k">this</span><span class="p">.</span><span class="nx">layer</span><span class="p">;</span></div><div class='line' id='LC701'><br/></div><div class='line' id='LC702'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">if</span> <span class="p">(</span><span class="k">this</span><span class="p">.</span><span class="nx">deviceIsAndroid</span><span class="p">)</span> <span class="p">{</span></div><div class='line' id='LC703'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nx">layer</span><span class="p">.</span><span class="nx">removeEventListener</span><span class="p">(</span><span class="s1">&#39;mouseover&#39;</span><span class="p">,</span> <span class="k">this</span><span class="p">.</span><span class="nx">onMouse</span><span class="p">,</span> <span class="kc">true</span><span class="p">);</span></div><div class='line' id='LC704'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nx">layer</span><span class="p">.</span><span class="nx">removeEventListener</span><span class="p">(</span><span class="s1">&#39;mousedown&#39;</span><span class="p">,</span> <span class="k">this</span><span class="p">.</span><span class="nx">onMouse</span><span class="p">,</span> <span class="kc">true</span><span class="p">);</span></div><div class='line' id='LC705'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nx">layer</span><span class="p">.</span><span class="nx">removeEventListener</span><span class="p">(</span><span class="s1">&#39;mouseup&#39;</span><span class="p">,</span> <span class="k">this</span><span class="p">.</span><span class="nx">onMouse</span><span class="p">,</span> <span class="kc">true</span><span class="p">);</span></div><div class='line' id='LC706'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">}</span></div><div class='line' id='LC707'><br/></div><div class='line' id='LC708'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nx">layer</span><span class="p">.</span><span class="nx">removeEventListener</span><span class="p">(</span><span class="s1">&#39;click&#39;</span><span class="p">,</span> <span class="k">this</span><span class="p">.</span><span class="nx">onClick</span><span class="p">,</span> <span class="kc">true</span><span class="p">);</span></div><div class='line' id='LC709'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nx">layer</span><span class="p">.</span><span class="nx">removeEventListener</span><span class="p">(</span><span class="s1">&#39;touchstart&#39;</span><span class="p">,</span> <span class="k">this</span><span class="p">.</span><span class="nx">onTouchStart</span><span class="p">,</span> <span class="kc">false</span><span class="p">);</span></div><div class='line' id='LC710'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nx">layer</span><span class="p">.</span><span class="nx">removeEventListener</span><span class="p">(</span><span class="s1">&#39;touchmove&#39;</span><span class="p">,</span> <span class="k">this</span><span class="p">.</span><span class="nx">onTouchMove</span><span class="p">,</span> <span class="kc">false</span><span class="p">);</span></div><div class='line' id='LC711'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nx">layer</span><span class="p">.</span><span class="nx">removeEventListener</span><span class="p">(</span><span class="s1">&#39;touchend&#39;</span><span class="p">,</span> <span class="k">this</span><span class="p">.</span><span class="nx">onTouchEnd</span><span class="p">,</span> <span class="kc">false</span><span class="p">);</span></div><div class='line' id='LC712'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nx">layer</span><span class="p">.</span><span class="nx">removeEventListener</span><span class="p">(</span><span class="s1">&#39;touchcancel&#39;</span><span class="p">,</span> <span class="k">this</span><span class="p">.</span><span class="nx">onTouchCancel</span><span class="p">,</span> <span class="kc">false</span><span class="p">);</span></div><div class='line' id='LC713'><span class="p">};</span></div><div class='line' id='LC714'><br/></div><div class='line' id='LC715'><br/></div><div class='line' id='LC716'><span class="cm">/**</span></div><div class='line' id='LC717'><span class="cm"> * Check whether FastClick is needed.</span></div><div class='line' id='LC718'><span class="cm"> *</span></div><div class='line' id='LC719'><span class="cm"> * @param {Element} layer The layer to listen on</span></div><div class='line' id='LC720'><span class="cm"> */</span></div><div class='line' id='LC721'><span class="nx">FastClick</span><span class="p">.</span><span class="nx">notNeeded</span> <span class="o">=</span> <span class="kd">function</span><span class="p">(</span><span class="nx">layer</span><span class="p">)</span> <span class="p">{</span></div><div class='line' id='LC722'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="s1">&#39;use strict&#39;</span><span class="p">;</span></div><div class='line' id='LC723'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="kd">var</span> <span class="nx">metaViewport</span><span class="p">;</span></div><div class='line' id='LC724'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="kd">var</span> <span class="nx">chromeVersion</span><span class="p">;</span></div><div class='line' id='LC725'><br/></div><div class='line' id='LC726'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">// Devices that don&#39;t support touch don&#39;t need FastClick</span></div><div class='line' id='LC727'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">if</span> <span class="p">(</span><span class="k">typeof</span> <span class="nb">window</span><span class="p">.</span><span class="nx">ontouchstart</span> <span class="o">===</span> <span class="s1">&#39;undefined&#39;</span><span class="p">)</span> <span class="p">{</span></div><div class='line' id='LC728'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">return</span> <span class="kc">true</span><span class="p">;</span></div><div class='line' id='LC729'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">}</span></div><div class='line' id='LC730'><br/></div><div class='line' id='LC731'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">// Chrome version - zero for other browsers</span></div><div class='line' id='LC732'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nx">chromeVersion</span> <span class="o">=</span> <span class="o">+</span><span class="p">(</span><span class="sr">/Chrome\/([0-9]+)/</span><span class="p">.</span><span class="nx">exec</span><span class="p">(</span><span class="nx">navigator</span><span class="p">.</span><span class="nx">userAgent</span><span class="p">)</span> <span class="o">||</span> <span class="p">[,</span><span class="mi">0</span><span class="p">])[</span><span class="mi">1</span><span class="p">];</span></div><div class='line' id='LC733'><br/></div><div class='line' id='LC734'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">if</span> <span class="p">(</span><span class="nx">chromeVersion</span><span class="p">)</span> <span class="p">{</span></div><div class='line' id='LC735'><br/></div><div class='line' id='LC736'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">if</span> <span class="p">(</span><span class="nx">FastClick</span><span class="p">.</span><span class="nx">prototype</span><span class="p">.</span><span class="nx">deviceIsAndroid</span><span class="p">)</span> <span class="p">{</span></div><div class='line' id='LC737'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nx">metaViewport</span> <span class="o">=</span> <span class="nb">document</span><span class="p">.</span><span class="nx">querySelector</span><span class="p">(</span><span class="s1">&#39;meta[name=viewport]&#39;</span><span class="p">);</span></div><div class='line' id='LC738'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;</div><div class='line' id='LC739'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">if</span> <span class="p">(</span><span class="nx">metaViewport</span><span class="p">)</span> <span class="p">{</span></div><div class='line' id='LC740'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">// Chrome on Android with user-scalable=&quot;no&quot; doesn&#39;t need FastClick (issue #89)</span></div><div class='line' id='LC741'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">if</span> <span class="p">(</span><span class="nx">metaViewport</span><span class="p">.</span><span class="nx">content</span><span class="p">.</span><span class="nx">indexOf</span><span class="p">(</span><span class="s1">&#39;user-scalable=no&#39;</span><span class="p">)</span> <span class="o">!==</span> <span class="o">-</span><span class="mi">1</span><span class="p">)</span> <span class="p">{</span></div><div class='line' id='LC742'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">return</span> <span class="kc">true</span><span class="p">;</span></div><div class='line' id='LC743'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">}</span></div><div class='line' id='LC744'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">// Chrome 32 and above with width=device-width or less don&#39;t need FastClick</span></div><div class='line' id='LC745'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">if</span> <span class="p">(</span><span class="nx">chromeVersion</span> <span class="o">&gt;</span> <span class="mi">31</span> <span class="o">&amp;&amp;</span> <span class="nb">window</span><span class="p">.</span><span class="nx">innerWidth</span> <span class="o">&lt;=</span> <span class="nb">window</span><span class="p">.</span><span class="nx">screen</span><span class="p">.</span><span class="nx">width</span><span class="p">)</span> <span class="p">{</span></div><div class='line' id='LC746'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">return</span> <span class="kc">true</span><span class="p">;</span></div><div class='line' id='LC747'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">}</span></div><div class='line' id='LC748'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">}</span></div><div class='line' id='LC749'><br/></div><div class='line' id='LC750'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">// Chrome desktop doesn&#39;t need FastClick (issue #15)</span></div><div class='line' id='LC751'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">}</span> <span class="k">else</span> <span class="p">{</span></div><div class='line' id='LC752'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">return</span> <span class="kc">true</span><span class="p">;</span></div><div class='line' id='LC753'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">}</span></div><div class='line' id='LC754'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">}</span></div><div class='line' id='LC755'><br/></div><div class='line' id='LC756'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">// IE10 with -ms-touch-action: none, which disables double-tap-to-zoom (issue #97)</span></div><div class='line' id='LC757'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">if</span> <span class="p">(</span><span class="nx">layer</span><span class="p">.</span><span class="nx">style</span><span class="p">.</span><span class="nx">msTouchAction</span> <span class="o">===</span> <span class="s1">&#39;none&#39;</span><span class="p">)</span> <span class="p">{</span></div><div class='line' id='LC758'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">return</span> <span class="kc">true</span><span class="p">;</span></div><div class='line' id='LC759'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">}</span></div><div class='line' id='LC760'><br/></div><div class='line' id='LC761'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">return</span> <span class="kc">false</span><span class="p">;</span></div><div class='line' id='LC762'><span class="p">};</span></div><div class='line' id='LC763'><br/></div><div class='line' id='LC764'><br/></div><div class='line' id='LC765'><span class="cm">/**</span></div><div class='line' id='LC766'><span class="cm"> * Factory method for creating a FastClick object</span></div><div class='line' id='LC767'><span class="cm"> *</span></div><div class='line' id='LC768'><span class="cm"> * @param {Element} layer The layer to listen on</span></div><div class='line' id='LC769'><span class="cm"> */</span></div><div class='line' id='LC770'><span class="nx">FastClick</span><span class="p">.</span><span class="nx">attach</span> <span class="o">=</span> <span class="kd">function</span><span class="p">(</span><span class="nx">layer</span><span class="p">)</span> <span class="p">{</span></div><div class='line' id='LC771'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="s1">&#39;use strict&#39;</span><span class="p">;</span></div><div class='line' id='LC772'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">return</span> <span class="k">new</span> <span class="nx">FastClick</span><span class="p">(</span><span class="nx">layer</span><span class="p">);</span></div><div class='line' id='LC773'><span class="p">};</span></div><div class='line' id='LC774'><br/></div><div class='line' id='LC775'><br/></div><div class='line' id='LC776'><span class="k">if</span> <span class="p">(</span><span class="k">typeof</span> <span class="nx">define</span> <span class="o">!==</span> <span class="s1">&#39;undefined&#39;</span> <span class="o">&amp;&amp;</span> <span class="nx">define</span><span class="p">.</span><span class="nx">amd</span><span class="p">)</span> <span class="p">{</span></div><div class='line' id='LC777'><br/></div><div class='line' id='LC778'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="c1">// AMD. Register as an anonymous module.</span></div><div class='line' id='LC779'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nx">define</span><span class="p">(</span><span class="kd">function</span><span class="p">()</span> <span class="p">{</span></div><div class='line' id='LC780'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="s1">&#39;use strict&#39;</span><span class="p">;</span></div><div class='line' id='LC781'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="k">return</span> <span class="nx">FastClick</span><span class="p">;</span></div><div class='line' id='LC782'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">});</span></div><div class='line' id='LC783'><span class="p">}</span> <span class="k">else</span> <span class="k">if</span> <span class="p">(</span><span class="k">typeof</span> <span class="nx">module</span> <span class="o">!==</span> <span class="s1">&#39;undefined&#39;</span> <span class="o">&amp;&amp;</span> <span class="nx">module</span><span class="p">.</span><span class="nx">exports</span><span class="p">)</span> <span class="p">{</span></div><div class='line' id='LC784'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nx">module</span><span class="p">.</span><span class="nx">exports</span> <span class="o">=</span> <span class="nx">FastClick</span><span class="p">.</span><span class="nx">attach</span><span class="p">;</span></div><div class='line' id='LC785'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nx">module</span><span class="p">.</span><span class="nx">exports</span><span class="p">.</span><span class="nx">FastClick</span> <span class="o">=</span> <span class="nx">FastClick</span><span class="p">;</span></div><div class='line' id='LC786'><span class="p">}</span> <span class="k">else</span> <span class="p">{</span></div><div class='line' id='LC787'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nb">window</span><span class="p">.</span><span class="nx">FastClick</span> <span class="o">=</span> <span class="nx">FastClick</span><span class="p">;</span></div><div class='line' id='LC788'><span class="p">}</span></div></pre></div>
            </td>
          </tr>
        </table>
  </div>

  </div>
</div>

<a href="#jump-to-line" rel="facebox[.linejump]" data-hotkey="l" class="js-jump-to-line" style="display:none">Jump to Line</a>
<div id="jump-to-line" style="display:none">
  <form accept-charset="UTF-8" class="js-jump-to-line-form">
    <input class="linejump-input js-jump-to-line-field" type="text" placeholder="Jump to line&hellip;" autofocus>
    <button type="submit" class="button">Go</button>
  </form>
</div>

        </div>

      </div><!-- /.repo-container -->
      <div class="modal-backdrop"></div>
    </div><!-- /.container -->
  </div><!-- /.site -->


    </div><!-- /.wrapper -->

      <div class="container">
  <div class="site-footer">
    <ul class="site-footer-links right">
      <li><a href="https://status.github.com/">Status</a></li>
      <li><a href="http://developer.github.com">API</a></li>
      <li><a href="http://training.github.com">Training</a></li>
      <li><a href="http://shop.github.com">Shop</a></li>
      <li><a href="/blog">Blog</a></li>
      <li><a href="/about">About</a></li>

    </ul>

    <a href="/">
      <span class="mega-octicon octicon-mark-github"></span>
    </a>

    <ul class="site-footer-links">
      <li>&copy; 2013 <span title="0.04253s from github-fe128-cp1-prd.iad.github.net">GitHub</span>, Inc.</li>
        <li><a href="/site/terms">Terms</a></li>
        <li><a href="/site/privacy">Privacy</a></li>
        <li><a href="/security">Security</a></li>
        <li><a href="/contact">Contact</a></li>
    </ul>
  </div><!-- /.site-footer -->
</div><!-- /.container -->


    <div class="fullscreen-overlay js-fullscreen-overlay" id="fullscreen_overlay">
  <div class="fullscreen-container js-fullscreen-container">
    <div class="textarea-wrap">
      <textarea name="fullscreen-contents" id="fullscreen-contents" class="js-fullscreen-contents" placeholder="" data-suggester="fullscreen_suggester"></textarea>
          <div class="suggester-container">
              <div class="suggester fullscreen-suggester js-navigation-container" id="fullscreen_suggester"
                 data-url="/ftlabs/fastclick/suggestions/commit">
              </div>
          </div>
    </div>
  </div>
  <div class="fullscreen-sidebar">
    <a href="#" class="exit-fullscreen js-exit-fullscreen tooltipped leftwards" title="Exit Zen Mode">
      <span class="mega-octicon octicon-screen-normal"></span>
    </a>
    <a href="#" class="theme-switcher js-theme-switcher tooltipped leftwards"
      title="Switch themes">
      <span class="octicon octicon-color-mode"></span>
    </a>
  </div>
</div>



    <div id="ajax-error-message" class="flash flash-error">
      <span class="octicon octicon-alert"></span>
      <a href="#" class="octicon octicon-remove-close close ajax-error-dismiss"></a>
      Something went wrong with that request. Please try again.
    </div>

  </body>
</html>

