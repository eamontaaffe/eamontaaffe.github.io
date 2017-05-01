import React, { Component } from 'react'
import { connect } from 'react-redux';

import { fetchPosts } from '../actions/';
import Posts from '../components/Posts';

class Blog extends Component {
    componentDidMount() {
	const { dispatch } = this.props
	dispatch(fetchPosts())
    }

    render() {
	return (
		<div className="Blog">
		<Posts posts={this.props.content} />
		</div>
	)
    }
}


const mapStateToProps = (state) => {
    const { posts } = state
    return {
	...posts,
    }
}

export default connect(mapStateToProps)(Blog);
