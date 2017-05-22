import React, { Component } from 'react';
import { connect } from 'react-redux';

import Post from '../components/Post';
import Fetching from '../components/Fetching';
import { fetchPosts } from '../actions';

class Posts extends Component {
    componentDidMount() {
        this.props.onLoad()
    }
    renderContent(content) {
        return (
	        <ul className="Posts">
	        {content.map((post, i) => (
		        <Post
	            key={i}
	            {...post}
		        />
	        )
                            )}
            </ul>
        )
    }
    
    render() {
        const { isFetching, content } = this.props;
        return (
                <div className="Posts">
                {isFetching ? <Fetching /> : this.renderContent(content)}
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

const mapDispatchToProps = (dispatch) => {
    return {
        onLoad: () => dispatch(fetchPosts())
    }
}

export default connect(mapStateToProps,mapDispatchToProps)(Posts);
