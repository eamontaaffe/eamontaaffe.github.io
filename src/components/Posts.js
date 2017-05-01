import React from 'react';

import Post from './Post';

const Posts = (props) => (
	<ul className="Posts">
	{props.posts.map((post, i) => (
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

export default Posts;
