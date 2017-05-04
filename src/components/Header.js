import React from 'react';
import { Link } from 'react-router';

const headingWidth = 45;

function asciiGap(len) {
    return "\u200B\u00A0".repeat(len)
}

function renderLink(pathname, heading) {
    if (pathname === "/") {
	return null
    }
    return (
	<span>
	    {asciiGap(headingWidth - heading.length - 2)}
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
