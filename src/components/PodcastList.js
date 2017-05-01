import React from 'react';
import Podcast from './Podcast';

const PodcastList = ({podcasts}) => (
	<div className="PodcastList">
	{podcasts.map(
	    (podcast, i) =>
		<Podcast key={i} podcast={podcast}/>
	)}
    </div>
);

export default PodcastList;
