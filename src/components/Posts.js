import React from 'react';

import Post from './Post';
import Fetching from './Fetching';

function renderContent(content) {
    return (
	    <ul className="Posts">
	    {content.map((post, i) => (
		    <Post
	        key={i}
	        title={post.title}
	        date={post.date}
	        content={post.content}
		    />
	    )
                        )}
        </ul>
    )
}

const Posts = ({isFetching, content}) => (
    <div className="Posts">
        {isFetching ? <Fetching /> : renderContent(content)}
    </div>
)

export default Posts;
