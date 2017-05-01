import React from 'react';
import { Link } from 'react-router';

const headingWidth = 45;

function fillArray(value, len) {
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr.push(value);
  }
  return arr;
}

function renderLink(pathname, heading) {
    if (pathname === "/") {
	return null
    }
    return (
	<span>
	    {fillArray((<span>&#8203;&nbsp;</span>),headingWidth - heading.length - 2)}
	    <Link to="/">{"↩"}</Link>
	</span>
    )
}

const Header = ({heading, pathname}) => (
	<div className="Header">
	{heading}{""}{renderLink(pathname, heading)}<br/>
	{"=".repeat(headingWidth)} <br/>
	</div>
)

export default Header;
