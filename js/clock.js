/**
 * A clock which displays a specific timezone no matter where it it is rendered
 * in the world.
 *
 * It is built using the Model, View, Update (MVU) pattern. This is a similar
 * pattern to what you would find in elm, however ther is only vanilla js being
 * used.
 *
 * To be honest, it is a bit of overkill. You do not need to write this many
 * lines of code to make a clock tick. But it was a good experience writing in
 * a functional style without using a framework. It is interesting to think
 * about how you can use regular object to represent algebraic data types.
 * Immitating some of the funcionality you get for free in a framework like elm
 * such as subscribing to time updates was an interesting experience too.
 *
 * @module clock
 */

/*** TYPES ***/

/**
 * Used to represent an optional value.
 * @type {maybe a}
 */
var maybe = {
  /**
   * Construct an empty value.
   * @returns {maybe a}
   */
  nothing: function() {
    return {
      type: "Maybe",
      constructor: "Nothing",
    };
  },

  /**
   * Construct a present value.
   * @param {a} x - The value to be instantiated.
   * @returns {maybe a}
   */
  just: function(x) {
    return {
      type: "Maybe",
      constructor: "Just",
      value: x,
    };
  },

  /**
   * Retrieve the kind of constructor used.
   * @param {maybe a} x - A maybe type.
   * @returns {string} - The string representation of the constructor.
   */
  constructor: function(x) {
    return x.constructor;
  },

  /**
   * Retrieve the value contained in the maybe type.
   * @param {maybe a} x - A maybe type.
   * @returns {a} - The value contained in the maybe.
   */
  value: function(x) {
    return x.value;
  },
};

var event = {
  tick: function(date) {
    return {
      type: "Tick",
      value: date,
    };
  },

  type: function(x) {
    return x.type;
  },

  value: function(x) {
    return x.value;
  }
};

/*** MODEL ***/

/**
 * The state of the system.
 * @type {{ date: maybe Date, offset: number, zone: string }} state
 */

/**
 * Build the initial state.
 * @param {number} offset - The offset in hours from UTC.
 * @param {string} zone - The timezone string.
 * @returns {state}
 */
var initialState = function(offset, zone) {
  return {
    date: maybe.nothing(),
    offset: offset,
    zone: zone,
  };
};

/*** VIEW ***/

/**
 * Convert a number into a string with a specific number of characters, use 0
 * to pad any unused space.
 * @param {number} x - The number which we want to pad.
 * @param {number} l - The desired length of the string output.
 * @returns {string} The padded numeric string.
 */
var padDigit = function(x, l) {
  return x
    .toString()
    .padStart(l, "0");
};

/**
 * Format a time into a string.
 * @param {Date} client - The date to be formatted.
 * @param {number} offset - The timezone offset in hours from UTC.
 * @param {string} zone - The timezone string.
 */
var formatTime = function(client, offset, zone) {
  

  let tzDiff = offset * 60 + client.getTimezoneOffset();
  let now = new Date(client.getTime() + tzDiff * 60 * 1000);

  
  let h = now.getHours();
  let H = padDigit(h, 2);
  let m = now.getMinutes();
  let M = padDigit(m, 2);
  let s = now.getSeconds();
  let S = padDigit(s, 2);
  
  return H + ":" +
    M + ":" +
    S + " " +
    zone;
};

/**
 * Take the current state and turn it into the innerHTML to be rendered.
 * @param {maybe Date} state - The current state of the system.
 * @returns {string} The innerHTML to be rendered.
 */
var view = function(state) {
  var date = state.date;
  var zone = state.zone;
  var offset = state.offset;
  
  switch(maybe.constructor(date)) {
  case "Just":
    return formatTime(maybe.value(date), offset, zone);
  case "Nothing":
  default:
    return "__:__:__ ____";
  };
};

/*** UPDATE ***/

/**
 * Update the state based on a new event
 * @param {maybe Date} state - The current state.
 * @param {event} e - An event.
 * @returns {maybe Date} The next state.
 */
var update = function(state, e) {
  return {
    offset: state.offset,
    zone: state.zone,
    date: maybe.just(event.value(e)),
  };
};

/*** MAIN ***/

/**
 * Tick the tock
 * @param {Element} el - The root element to be modified
 * @param {state} state - The current state of the system.
 */
var tick = function (el, state) {
  return function() {
    let e = event.tick(new Date());
    let nextState = update(state, e);
    let innerHTML = view(nextState);

    el.innerHTML = innerHTML;

    window.setTimeout(tick(el, nextState), 500);
  };
};

/**
 * Used to instantiate the system onto an element.
 * @param {Element} - The root element.
 * @param {number} - The timezone offset in hours from UTC.
 * @param {string} - The timezone string.
 */
var renderClock = function(el, offset, zone) {
  var state = initialState(offset, zone);
  var innerHTML = view(state);
  el.innerHTML = innerHTML;

  /* Start ticking */
  tick(el, state)();
};
