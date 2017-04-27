import React, { Component } from 'react'


function prefix(level) {
    switch (level) {
    case 0:
	return ""
    default:
	return ("│ ".repeat(level-1) + "├── ")
    }
}

const Folder = (props) => {
    return (
	<span className="Folder">
	{prefix(props.level)}<a href="#">{props.name}/</a><br />
	</span>
    )
}

export default Folder;
