import React from 'react';
import marked from 'marked';

import Fetching from './Fetching';

function renderContent(content) {
    return (
            <div
        className="About"
        dangerouslySetInnerHTML={{__html: marked(content)}}
	    />
    )
}

const About = ({content, isFetching}) => {
    return (
        <div className="About" >
            {isFetching ? <Fetching /> : renderContent(content)}
        </div>
    )
}

export default About;
