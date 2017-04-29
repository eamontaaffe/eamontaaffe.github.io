import React from 'react'
import { connect } from 'react-redux';

import Posts from '../components/Posts'

const Blog = (props) => (
	<div className="Blog">
	<Posts posts={props.posts} />
	</div>
)

const mapStateToProps = (state) => {
    const { posts } = state
    return {
	posts: posts
    }
}

export default connect(mapStateToProps)(Blog);
