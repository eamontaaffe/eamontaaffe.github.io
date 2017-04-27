import React from 'react'
import Folder from './Folder';

const LastSubFolder = (props) => (
    <span className="LastSubFolder">
        {"└── "}<Folder name={props.name} />
    </span>
)

export default LastSubFolder;
