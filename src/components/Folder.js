import React from 'react';
import { Link } from 'react-router';

const Folder = (props) => {
    return (
	    <span className={"Folder"}>
	    <li className={`Folder${props.isLast ? " Folder-last":""}`}>
	    <Link to={props.path}>{props.name}</Link>
	    </li>
	    {props.children}
	</span>
    )
}

export default Folder;
