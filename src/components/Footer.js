import React from 'react';
import { Link } from 'react-router';

const Footer = () => (
	<div className="Footer">
	<hr style={{height:"0pt", visibility:"hidden"}} />
	------<br/>
	<Link to="/">{"↩"}</Link>
	</div>
)

export default Footer;
