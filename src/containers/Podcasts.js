import React from 'react';
import { connect } from 'react-redux';

import PodcastList from '../components/PodcastList';

const Podcasts = ({podcasts}) => (
	<PodcastList podcasts={podcasts} />
)

const mapStateToProps = (state) => {
    const {podcasts} = state;
    return {
	podcasts
    }
}

export default connect(mapStateToProps)(Podcasts);
