
function toggleDropDown(eventElement)
{var choices=$get('SearchChoices');if(isDropDownVisible)
{choices.style.display='none';isDropDownVisible=false;}
else
{choices.style.display='block';isDropDownVisible=true;var clickEvent=function(e){if(isDropDownVisible&&e.target.id!="SearchIcon"&&e.target.id.indexOf("downArrow")==-1){toggleDropDown();$(document).unbind("click",clickEvent);}}
$(document).bind("click",clickEvent);}}
function selectSearch(eventElement)
{toggleDropDown(eventElement);$get('SearchIcon').style.backgroundImage=dnn.getVar(eventElement.target.id+'Url');if(eventElement.target.id.indexOf("Web")>0)
{dnn.setVar('SearchIconSelected','W');}
else
{dnn.setVar('SearchIconSelected','S');}}
function searchHilite(eventElement)
{eventElement.target.className='searchHilite';}
function searchDefault(eventElement)
{eventElement.target.className='searchDefault';}
function initSearch()
{var searchIcon=$get('SearchIcon');if(dnn.getVar('SearchIconSelected')=='S')
{searchIcon.style.backgroundImage=dnn.getVar('SearchIconSiteUrl');}
else
{searchIcon.style.backgroundImage=dnn.getVar('SearchIconWebUrl');}
$addHandler(searchIcon,'click',toggleDropDown);var siteIcon=$get('SearchIconSite');siteIcon.style.backgroundImage=dnn.getVar('SearchIconSiteUrl');$addHandler(siteIcon,'click',selectSearch);$addHandler(siteIcon,'mouseover',searchHilite);$addHandler(siteIcon,'mouseout',searchDefault);var webIcon=$get('SearchIconWeb');webIcon.style.backgroundImage=dnn.getVar('SearchIconWebUrl');$addHandler(webIcon,'click',selectSearch);$addHandler(webIcon,'mouseover',searchHilite);$addHandler(webIcon,'mouseout',searchDefault);$get('SearchChoices').style.display='none';}
var isDropDownVisible=false;initSearch();
;;;