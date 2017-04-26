import React from 'react';

import Folder from './folder';

const SubFolder = (props) => (
    <span className="SubFolder">
	{"├── "}<Folder name={props.name} />
    </span>
)

export default SubFolder
