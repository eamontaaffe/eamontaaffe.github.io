import React from 'react';
import Podcast from './Podcast';

const PodcastList = ({content}) => (
	<div className="PodcastList">
	{content.map(
	    (podcast, i) =>
		<Podcast key={i} podcast={podcast}/>
	)}
    </div>
);

export default PodcastList;
