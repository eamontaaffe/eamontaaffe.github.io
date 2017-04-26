import React from 'react'
import Folder from './folder';

const LastSubFolder = (props) => (
	<span class="LastSubFolder">{"└── "}<Folder name={props.name} /></span>
)

export default LastSubFolder;
