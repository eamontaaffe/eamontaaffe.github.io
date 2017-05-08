import React from 'react';

const footerWidth = 60

const Footer = ({ pathname }) => (
	<div className="Footer">
	<hr style={{height:"0pt", visibility:"hidden"}} />
	<span className="nowrap">{"-".repeat(footerWidth)}</span>
        <br/>
	</div>
)

export default Footer;
