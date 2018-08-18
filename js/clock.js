function padDigit(x, l) {
  return x
    .toString()
    .padStart(l, "0");
}

function updateClock(offset, zone) {
  let client = new Date();

  let tzDiff = offset * 60 + client.getTimezoneOffset();
  let now = new Date(client.getTime() + tzDiff * 60 * 1000);


  let h = now.getHours();
  let H = padDigit(h, 2);
  let m = now.getMinutes();
  let M = padDigit(m, 2);
  let s = now.getSeconds();
  let S = padDigit(s, 2);

  document.getElementById("clock").innerHTML =
    H + ":" +
    M + ":" +
    S + " " +
    zone;

  setTimeout(function() { updateClock(offset, zone); }, 500);
}
