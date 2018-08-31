var forEachElement = function (els, fn) {
  for (var i = 0; i < els.length; i++) {
    fn(els.item(i));
  }
};

/******************************************************************************/

var findCountryThen = function (code, fn) {
  var el = document.getElementById("country-" + code);
  if (el) {
    return fn (el);
  }
  return null;
};

var onLandMouseover = function (event) {
  return findCountryThen(
    event.target.id,
    function (el) {
      return el.className += " active";
    },
  );
    
};

var onLandMouseout = function (event) {
  return findCountryThen(
    event.target.id,
    function(el) {
      el.className = el.className.replace(" active", ""); 
    }
  );
};

var onLandClick = function (event) {
  return alert(event.target.getAttribute("title"));
};

var landUpdate = function (el) {
  el.addEventListener(
    "mouseover",
    onLandMouseover,
  );

  el.addEventListener(
    "mouseout",
    onLandMouseout,
  );

  el.addEventListener(
    "click",
    onLandClick,
  );
};

var lands = document.getElementsByClassName("land");

forEachElement(lands, landUpdate);

/******************************************************************************/

var getCountryThen = function (code, fn) {
  var el = document.getElementById(code);

  if (el) {
    return fn(el);
  }

  return null;
};

var onCountryMouseover = function (event) {
  var code = event
      .target
      .id
      .replace("country-", "");

  return getCountryThen(
    code,
    function (el) { el.className.baseVal += " active"; },
  );
};

var onCountryMouseout = function (event) {
  var code = event
      .target
      .id
      .replace("country-", "");

  return getCountryThen(
    code,
    function (el) {
      el.className.baseVal = el.className.baseVal.replace(" active", "");
    },
  );
};

var countryUpdate = function (el) {
  el.addEventListener(
    "mouseover",
    onCountryMouseover,
  );

  el.addEventListener(
    "mouseout",
    onCountryMouseout,
  );
};

var countries = document.getElementsByClassName("country");

forEachElement(countries, countryUpdate);
