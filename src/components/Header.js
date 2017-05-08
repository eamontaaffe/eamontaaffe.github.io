import React from 'react';
import { Link } from 'react-router';

const headingWidth = 60;

function renderLink(pathname, heading) {
    if (pathname === "/") {
	return null
    }
    return (
	<span>
	    {"\u00A0"}<Link to="/">{"↩"}</Link>
	</span>
    )
}

const Header = ({heading, pathname}) => (
	<div className="Header">
	{heading}{""}{renderLink(pathname, heading)}<br/>
	<span className="nowrap">{"=\u200B".repeat(headingWidth)}</span>
        <br/>
	</div>
)

export default Header;
