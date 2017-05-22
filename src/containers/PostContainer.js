import React, { Component } from 'react';
import { connect } from 'react-redux';

import Post from '../components/Post';
import Fetching from '../components/Fetching';
import { fetchPosts } from '../actions';

class PostContainer extends Component {
    componentDidMount() {
        this.props.dispatch(fetchPosts())
    }

    render() {
        const { noMatch } = this.props;
        return noMatch ? <Fetching /> : <Post {...this.props} />
    }

}

const mapStateToProps = (state, ownProps) => {
    const { id } = ownProps.match.params;
    const { posts } = state;
    const post = posts.content.find((post) => (post.id === id))
    const noMatch = !post;
    return {
        ...ownProps,
        noMatch,
        ...post
    }
}

const mapDispatchToProps = (dispatch) => dispatch;

export default connect(
    mapStateToProps,
)(PostContainer);
