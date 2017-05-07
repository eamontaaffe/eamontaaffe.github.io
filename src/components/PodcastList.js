import React from 'react';

import Fetching from './Fetching';
import Podcast from './Podcast';

function renderList(content) {
    return (
        content.map(
	    (podcast, i) =>
		<Podcast key={i} podcast={podcast}/>
	)
    )
}

function renderFetching() {
    return <Fetching />
}

const PodcastList = ({content, isFetching}) => (
	<div className="PodcastList">
	{isFetching ? renderFetching() : renderList(content)}
    </div>
);

export default PodcastList;
