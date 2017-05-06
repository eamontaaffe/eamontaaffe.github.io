import React, { Component } from 'react';
import { connect } from 'react-redux';

import { fetchPodcasts } from '../actions';
import PodcastList from '../components/PodcastList';

class Podcasts extends Component {

    componentDidMount() {
        const { dispatch } = this.props
        dispatch(fetchPodcasts())
    }

    render() {
        return (
	        <PodcastList {...this.props.podcasts} />
        )
    }
}

const mapStateToProps = (state) => {
    const {podcasts} = state;
    return {
	podcasts
    }
}

export default connect(mapStateToProps)(Podcasts);
