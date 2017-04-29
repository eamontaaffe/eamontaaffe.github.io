import React from 'react';

import Folder from './Folder';

function renderDir(dir, id=1, level=0, isLast=false, path="") {
    const head = dir.find(dir => dir.id === id)
    const contents = head.contents || []
    const name = head.alias || head.name
    const curr_path = path + head.name

    return (
	    <Folder
	name={name}
	key={head.id}
	level={level}
	isLast={isLast}
	path={curr_path}
	    >
	    <ul>
	    {contents.reduce(
		(acc, val, i) => acc.concat(
		    renderDir(
			dir,
			val,
			level+1,
			(i === contents.length - 1),
			curr_path
		    )),
		[])
	    }
	</ul>
	    </Folder>
    )	
}

const Directory = (props) => (
	<div className="Directory">
	<ul>
	{renderDir(props.dir)}
    </ul>
	</div>
)

export default Directory
